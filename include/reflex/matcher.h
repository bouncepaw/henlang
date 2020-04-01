/******************************************************************************\
* Copyright (c) 2016, Robert van Engelen, Genivia Inc. All rights reserved.    *
*                                                                              *
* Redistribution and use in source and binary forms, with or without           *
* modification, are permitted provided that the following conditions are met:  *
*                                                                              *
*   (1) Redistributions of source code must retain the above copyright notice, *
*       this list of conditions and the following disclaimer.                  *
*                                                                              *
*   (2) Redistributions in binary form must reproduce the above copyright      *
*       notice, this list of conditions and the following disclaimer in the    *
*       documentation and/or other materials provided with the distribution.   *
*                                                                              *
*   (3) The name of the author may not be used to endorse or promote products  *
*       derived from this software without specific prior written permission.  *
*                                                                              *
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED *
* WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF         *
* MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO   *
* EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,       *
* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, *
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;  *
* OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,     *
* WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR      *
* OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF       *
* ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                                   *
\******************************************************************************/

/**
@file      matcher.h
@brief     RE/flex matcher engine
@author    Robert van Engelen - engelen@genivia.com
@copyright (c) 2016-2019, Robert van Engelen, Genivia Inc. All rights reserved.
@copyright (c) BSD-3 License - see LICENSE.txt
*/

#ifndef REFLEX_MATCHER_H
#define REFLEX_MATCHER_H

#include <reflex/absmatcher.h>
#include <reflex/pattern.h>
#include <stack>

namespace reflex {

/// RE/flex matcher engine class, implements reflex::PatternMatcher pattern matching interface with scan, find, split functors and iterators.
/** More info TODO */
class Matcher : public PatternMatcher<reflex::Pattern> {
 public:
  /// Convert a regex to an acceptable form, given the specified regex library signature `"[decls:]escapes[?+]"`, see reflex::convert.
  template<typename T>
  static std::string convert(T regex, convert_flag_type flags = convert_flag::none)
  {
    return reflex::convert(regex, "imsx#=^:abcdefhijklnrstuvwxzABDHLNQSUW<>?+", flags);
  }
  /// Default constructor.
  Matcher() : PatternMatcher<reflex::Pattern>()
  {
    Matcher::reset();
  }
  /// Construct matcher engine from a pattern or a string regex, and an input character sequence.
  template<typename P> /// @tparam <P> a reflex::Pattern or a string regex 
  Matcher(
      const P     *pattern,         ///< points to a reflex::Pattern or a string regex for this matcher
      const Input& input = Input(), ///< input character sequence for this matcher
      const char  *opt = NULL)      ///< option string of the form `(A|N|T(=[[:digit:]])?|;)*`
    :
      PatternMatcher<reflex::Pattern>(pattern, input, opt)
  {
    reset(opt);
  }
  /// Construct matcher engine from a pattern or a string regex, and an input character sequence.
  template<typename P> /// @tparam <P> a reflex::Pattern or a string regex 
  Matcher(
      const P&     pattern,          ///< a reflex::Pattern or a string regex for this matcher
      const Input& input = Input(),  ///< input character sequence for this matcher
      const char   *opt = NULL)      ///< option string of the form `(A|N|T(=[[:digit:]])?|;)*`
    :
      PatternMatcher<reflex::Pattern>(pattern, input, opt)
  {
    reset(opt);
  }
  /// Copy constructor.
  Matcher(const Matcher& matcher) ///< matcher to copy with pattern (pattern may be shared)
    :
      PatternMatcher<reflex::Pattern>(matcher),
      ded_(matcher.ded_),
      tab_(matcher.tab_)
  {
    bmd_ = matcher.bmd_;
    if (bmd_ != 0)
      std::memcpy(bms_, matcher.bms_, sizeof(bms_));
  }
  /// Assign a matcher.
  Matcher& operator=(const Matcher& matcher) ///< matcher to copy
  {
    PatternMatcher<reflex::Pattern>::operator=(matcher);
    ded_ = matcher.ded_;
    tab_ = matcher.tab_;
    bmd_ = matcher.bmd_;
    if (bmd_ != 0)
      std::memcpy(bms_, matcher.bms_, sizeof(bms_));
    return *this;
  }
  /// Polymorphic cloning.
  virtual Matcher *clone()
  {
    return new Matcher(*this);
  }
  /// Reset this matcher's state to the initial state.
  virtual void reset(const char *opt = NULL)
  {
    DBGLOG("Matcher::reset()");
    PatternMatcher<reflex::Pattern>::reset(opt);
    ded_ = 0;
    tab_.resize(0);
    bmd_ = 0;
  }
  virtual std::pair<const char*,size_t> operator[](size_t n) const
  {
    if (n == 0)
      return std::pair<const char*,size_t>(txt_, len_);
    return std::pair<const char*,size_t>(NULL, 0);
  }
  /// Returns the position of the last indent stop.
  size_t last_stop()
  {
    if (tab_.empty())
      return 0;
    return tab_.back();
  }
  /// Inserts or appends an indent stop position, keeping indent stops sorted.
  void insert_stop(size_t n)
  {
    if (n > 0)
    {
      if (tab_.empty() || tab_.back() < n)
      {
        tab_.push_back(n);
      }
      else
      {
        for (std::vector<size_t>::reverse_iterator i = tab_.rbegin(); i != tab_.rend(); ++i)
        {
          if (*i == n)
            return;
          if (*i < n)
          {
            tab_.insert(i.base(), n);
            return;
          }
        }
        tab_.insert(tab_.begin(), n);
      }
    }
  }
  /// Remove all stop positions from position n and up until the last.
  void delete_stop(size_t n)
  {
    if (!tab_.empty())
    {
      for (std::vector<size_t>::reverse_iterator i = tab_.rbegin(); i != tab_.rend(); ++i)
      {
        if (*i < n)
        {
          tab_.erase(i.base(), tab_.end());
          return;
        }
      }
      tab_.clear();
    }
  }
  /// Returns reference to vector of current indent stop positions.
  std::vector<size_t>& stops()
    /// @returns vector of size_t
  {
    return tab_;
  }
  /// Clear indent stop positions.
  void clear_stops()
  {
    tab_.clear();
  }
  /// Push current indent stops and clear current indent stops.
  void push_stops()
  {
    stk_.push(std::vector<size_t>());
    stk_.top().swap(tab_);
  }
  /// Pop indent stops.
  void pop_stops()
  {
    stk_.top().swap(tab_);
    stk_.pop();
  }
  /// FSM code INIT.
  inline void FSM_INIT(int& c1)
  {
    c1 = fsm_.c1;
  }
  /// FSM code FIND.
  inline void FSM_FIND()
  {
    if (cap_ == 0)
      cur_ = pos_;
  }
  /// FSM code CHAR.
  inline int FSM_CHAR()
  {
    return get();
  }
  /// FSM code HALT.
  inline void FSM_HALT(int c1 = AbstractMatcher::Const::UNK)
  {
    fsm_.c1 = c1;
  }
  /// FSM code TAKE.
  inline void FSM_TAKE(Pattern::Index cap)
  {
    cap_ = cap;
    cur_ = pos_;
  }
  /// FSM code TAKE.
  inline void FSM_TAKE(Pattern::Index cap, int c1)
  {
    cap_ = cap;
    cur_ = pos_;
    if (c1 != EOF)
      --cur_;
  }
  /// FSM code REDO.
  inline void FSM_REDO()
  {
    cap_ = Const::EMPTY;
    cur_ = pos_;
  }
  /// FSM code REDO.
  inline void FSM_REDO(int c1)
  {
    cap_ = Const::EMPTY;
    cur_ = pos_;
    if (c1 != EOF)
      --cur_;
  }
  /// FSM code HEAD.
  inline void FSM_HEAD(Pattern::Index la)
  {
    if (lap_.size() <= la && la < Pattern::Const::IMAX)
      lap_.resize(la + 1, -1);
    lap_[la] = static_cast<int>(pos_ - (txt_ - buf_));
  }
  /// FSM code TAIL.
  inline void FSM_TAIL(Pattern::Index la)
  {
    if (lap_.size() > la && lap_[la] >= 0)
      cur_ = txt_ - buf_ + static_cast<size_t>(lap_[la]);
  }
  /// FSM code DENT.
  inline bool FSM_DENT()
  {
    if (ded_ > 0)
    {
      fsm_.nul = true;
      return true;
    }
    return false;
  }
  /// FSM extra code POSN returns current position.
  inline size_t FSM_POSN()
  {
    return pos_ - (txt_ - buf_);
  }
  /// FSM extra code BACK position to a previous position returned by FSM_POSN().
  inline void FSM_BACK(size_t pos)
  {
    cur_ = txt_ - buf_ + pos;
  }
#if !defined(WITH_NO_INDENT)
  /// FSM code META DED.
  inline bool FSM_META_DED()
  {
    return fsm_.bol && dedent();
  }
  /// FSM code META IND.
  inline bool FSM_META_IND()
  {
    return fsm_.bol && indent();
  }
  /// FSM code META UND.
  inline bool FSM_META_UND()
  {
    bool mrk = mrk_ && !nodent();
    mrk_ = false;
    ded_ = 0;
    return mrk;
  }
#endif
  /// FSM code META EOB.
  inline bool FSM_META_EOB(int c1)
  {
    return c1 == EOF;
  }
  /// FSM code META BOB.
  inline bool FSM_META_BOB()
  {
    return at_bob();
  }
  /// FSM code META EOL.
  inline bool FSM_META_EOL(int c1)
  {
    return c1 == EOF || c1 == '\n';
  }
  /// FSM code META BOL.
  inline bool FSM_META_BOL()
  {
    return fsm_.bol;
  }
  /// FSM code META EWE.
  inline bool FSM_META_EWE(int c0, int c1)
  {
    return isword(c0) && !isword(c1);
  }
  /// FSM code META BWE.
  inline bool FSM_META_BWE(int c0, int c1)
  {
    return !isword(c0) && isword(c1);
  }
  /// FSM code META EWB.
  inline bool FSM_META_EWB()
  {
    return at_eow();
  }
  /// FSM code META BWB.
  inline bool FSM_META_BWB()
  {
    return at_bow();
  }
  /// FSM code META NWE.
  inline bool FSM_META_NWE(int c0, int c1)
  {
    return isword(c0) == isword(c1);
  }
  /// FSM code META NWB.
  inline bool FSM_META_NWB()
  {
    return !at_bow() && !at_eow();
  }
  /// Check CPU hardware for AVX512BW capability.
  static bool have_HW_AVX512BW()
  {
    return HW & (1ULL << 62);
  }
  /// Check CPU hardware for AVX capability.
  static bool have_HW_AVX()
  {
    return HW & (1ULL << 28);
  }
  /// Check CPU hardware for SSE2 capability.
  static bool have_HW_SSE2()
  {
    return HW & (1ULL << 26);
  }
 protected:
  typedef std::vector<size_t> Stops; ///< indent margin/tab stops
  /// FSM data for FSM code
  struct FSM {
    FSM() : bol(), nul(), c1() { }
    bool bol;
    bool nul;
    int  c1;
  };
  /// Get CPU hardware info.
  static uint64_t get_HW();
  /// CPU hardware info[2]
  static uint64_t HW;
  /// Returns true if input matched the pattern using method Const::SCAN, Const::FIND, Const::SPLIT, or Const::MATCH.
  virtual size_t match(Method method) ///< Const::SCAN, Const::FIND, Const::SPLIT, or Const::MATCH
    /// @returns nonzero if input matched the pattern
  {
    DBGLOG("BEGIN Matcher::match()");
    reset_text();
    len_ = 0; // split text length starts with 0
scan:
    txt_ = buf_ + cur_;
#if !defined(WITH_NO_INDENT)
    mrk_ = false;
    ind_ = pos_; // ind scans input in buf[] in newline() up to pos - 1
    col_ = 0; // count columns for indent matching
#endif
find:
    int c1 = got_;
    bool bol = at_bol();
    if (pat_->fsm_)
      fsm_.c1 = c1;
#if !defined(WITH_NO_INDENT)
redo:
#endif
    lap_.resize(0);
    cap_ = 0;
    bool nul = method == Const::MATCH;
    if (pat_->fsm_)
    {
      DBGLOG("FSM code %p", pat_->fsm_);
      fsm_.bol = bol;
      fsm_.nul = nul;
      pat_->fsm_(*this);
      nul = fsm_.nul;
      c1 = fsm_.c1;
    }
    else if (pat_->opc_)
    {
      const Pattern::Opcode *pc = pat_->opc_;
      while (true)
      {
        Pattern::Opcode opcode = *pc;
        DBGLOG("Fetch: code[%zu] = 0x%08X", pc - pat_->opc_, opcode);
        Pattern::Index index;
        if (Pattern::is_opcode_halt(opcode))
          break;
        if (Pattern::is_opcode_meta(opcode))
        {
          switch (opcode >> 16)
          {
            case 0xFF00: // TAKE
              cap_ = Pattern::index_of(opcode);
              DBGLOG("Take: cap = %zu", cap_);
              cur_ = pos_;
              ++pc;
              continue;
            case 0xFF7E: // TAIL
              index = Pattern::index_of(opcode);
              DBGLOG("Tail: %u", index);
              if (lap_.size() > index && lap_[index] >= 0)
                cur_ = txt_ - buf_ + static_cast<size_t>(lap_[index]); // mind the (new) gap
              ++pc;
              continue;
            case 0xFF7F: // HEAD
              index = Pattern::index_of(opcode);
              DBGLOG("Head: lookahead[%u] = %zu", index, pos_ - (txt_ - buf_));
              if (lap_.size() <= index)
                lap_.resize(index + 1, -1);
              lap_[index] = static_cast<int>(pos_ - (txt_ - buf_)); // mind the gap
              ++pc;
              continue;
#if !defined(WITH_NO_INDENT)
            case 0xFF00 | Pattern::META_DED:
              if (ded_ > 0)
              {
                index = Pattern::index_of(opcode);
                DBGLOG("Dedent ded = %zu", ded_); // unconditional dedent matching \j
                nul = true;
                pc = pat_->opc_ + index;
                continue;
              }
#endif
          }
          if (c1 == EOF)
            break;
          int c0 = c1;
          c1 = get();
          DBGLOG("Get: c1 = %d", c1);
          Pattern::Index back = Pattern::Const::IMAX; // where to jump back to (backtrack on meta transitions)
          Pattern::Index la;
          index = Pattern::Const::IMAX;
          while (true)
          {
            if (Pattern::is_opcode_meta(opcode) && (index == Pattern::Const::IMAX || back == Pattern::Const::IMAX))
            {
              // we no longer have to pass through all if index and back are set
              switch (opcode >> 16)
              {
                case 0xFF00: // TAKE
                  cap_ = Pattern::index_of(opcode);
                  DBGLOG("Take: cap = %zu", cap_);
                  cur_ = pos_;
                  if (c1 != EOF)
                    --cur_; // must unget one char
                  opcode = *++pc;
                  continue;
                case 0xFF7E: // TAIL
                  la = Pattern::index_of(opcode);
                  DBGLOG("Tail: %u", la);
                  if (lap_.size() > la && lap_[la] >= 0)
                    cur_ = txt_ - buf_ + static_cast<size_t>(lap_[la]); // mind the (new) gap
                  opcode = *++pc;
                  continue;
                case 0xFF7F: // HEAD
                  opcode = *++pc;
                  continue;
#if !defined(WITH_NO_INDENT)
                case 0xFF00 | Pattern::META_DED:
                  DBGLOG("DED? %d", c1);
                  if (index == Pattern::Const::IMAX && back == Pattern::Const::IMAX && bol && dedent())
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_IND:
                  DBGLOG("IND? %d", c1);
                  if (index == Pattern::Const::IMAX && back == Pattern::Const::IMAX && bol && indent())
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_UND:
                  DBGLOG("UND");
                  if (mrk_)
                    index = Pattern::index_of(opcode);
                  mrk_ = false;
                  ded_ = 0;
                  opcode = *++pc;
                  continue;
#endif
                case 0xFF00 | Pattern::META_EOB:
                  DBGLOG("EOB? %d", c1);
                  if (index == Pattern::Const::IMAX && c1 == EOF)
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_BOB:
                  DBGLOG("BOB? %d", at_bob());
                  if (index == Pattern::Const::IMAX && at_bob())
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_EOL:
                  DBGLOG("EOL? %d", c1);
                  if (index == Pattern::Const::IMAX && (c1 == EOF || c1 == '\n'))
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_BOL:
                  DBGLOG("BOL? %d", bol);
                  if (index == Pattern::Const::IMAX && bol)
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_EWE:
                  DBGLOG("EWE? %d %d %d", c0, c1, isword(c0) && !isword(c1));
                  if (index == Pattern::Const::IMAX && isword(c0) && !isword(c1))
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_BWE:
                  DBGLOG("BWE? %d %d %d", c0, c1, !isword(c0) && isword(c1));
                  if (index == Pattern::Const::IMAX && !isword(c0) && isword(c1))
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_EWB:
                  DBGLOG("EWB? %d", at_eow());
                  if (index == Pattern::Const::IMAX && at_eow())
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_BWB:
                  DBGLOG("BWB? %d", at_bow());
                  if (index == Pattern::Const::IMAX && at_bow())
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_NWE:
                  DBGLOG("NWE? %d %d %d", c0, c1, isword(c0) == isword(c1));
                  if (index == Pattern::Const::IMAX && isword(c0) == isword(c1))
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
                case 0xFF00 | Pattern::META_NWB:
                  DBGLOG("NWB? %d %d", at_bow(), at_eow());
                  if (index == Pattern::Const::IMAX && !at_bow() && !at_eow())
                    index = Pattern::index_of(opcode);
                  opcode = *++pc;
                  continue;
              }
            }
            if (index == Pattern::Const::IMAX)
            {
              if (back != Pattern::Const::IMAX)
              {
                pc = pat_->opc_ + back;
                opcode = *pc;
              }
              break;
            }
            DBGLOG("Backtrack: pc = %u", index);
            if (back == Pattern::Const::IMAX)
              back = static_cast<Pattern::Index>(pc - pat_->opc_);
            pc = pat_->opc_ + index;
            opcode = *pc;
            index = Pattern::Const::IMAX;
          }
          if (c1 == EOF)
            break;
        }
        else
        {
          if (c1 == EOF)
            break;
          c1 = get();
          DBGLOG("Get: c1 = %d", c1);
          if (c1 == EOF)
            break;
        }
        Pattern::Opcode lo = c1 << 24;
        Pattern::Opcode hi = lo | 0x00FFFFFF;
unrolled:
        if (hi < opcode || lo > (opcode << 8))
        {
          opcode = *++pc;
          if (hi < opcode || lo > (opcode << 8))
          {
            opcode = *++pc;
            if (hi < opcode || lo > (opcode << 8))
            {
              opcode = *++pc;
              if (hi < opcode || lo > (opcode << 8))
              {
                opcode = *++pc;
                if (hi < opcode || lo > (opcode << 8))
                {
                  opcode = *++pc;
                  if (hi < opcode || lo > (opcode << 8))
                  {
                    opcode = *++pc;
                    if (hi < opcode || lo > (opcode << 8))
                    {
                      opcode = *++pc;
                      if (hi < opcode || lo > (opcode << 8))
                      {
                        opcode = *++pc;
                        goto unrolled;
                      }
                    }
                  }
                }
              }
            }
          }
        }
        index = Pattern::index_of(opcode);
        if (index == Pattern::Const::IMAX)
          break;
        if (index == 0 && cap_ == 0) // failed to match so far, set cur_ to move forward from cur_ + 1 with FIND advance()
          cur_ = pos_;
        pc = pat_->opc_ + index;
      }
    }
#if !defined(WITH_NO_INDENT)
    if (mrk_ && cap_ != Const::EMPTY)
    {
      if (col_ > 0 && (tab_.empty() || tab_.back() < col_))
      {
        DBGLOG("Set new stop: tab_[%zu] = %zu", tab_.size(), col_);
        tab_.push_back(col_);
      }
      else if (!tab_.empty() && tab_.back() > col_)
      {
        size_t n;
        for (n = tab_.size() - 1; n > 0; --n)
          if (tab_.at(n - 1) <= col_)
            break;
        ded_ += tab_.size() - n;
        DBGLOG("Dedents: ded = %zu tab_ = %zu", ded_, tab_.size());
        tab_.resize(n);
        if (n > 0)
          tab_.back() = col_; // adjust stop when indents are not aligned (Python would give an error)
      }
    }
    if (ded_ > 0)
    {
      DBGLOG("Dedents: ded = %zu", ded_);
      if (col_ == 0 && bol)
      {
        ded_ += tab_.size();
        tab_.resize(0);
        DBGLOG("Rescan for pending dedents: ded = %zu", ded_);
        pos_ = ind_;
        bol = false; // avoid looping, match \j exactly
        goto redo;
      }
      --ded_;
    }
#endif
    if (method == Const::SPLIT)
    {
      DBGLOG("Split: len = %zu cap = %zu cur = %zu pos = %zu end = %zu txt-buf = %zu eob = %d got = %d", len_, cap_, cur_, pos_, end_, txt_-buf_, (int)eof_, got_);
      if (cap_ == 0 || (cur_ == static_cast<size_t>(txt_ - buf_) && !at_bob()))
      {
        if (!hit_end() && (txt_ + len_ < buf_ + end_ || peek() != EOF))
        {
          ++len_;
          DBGLOG("Split continue: len = %zu", len_);
          set_current(++cur_);
          goto find;
        }
        if (got_ != Const::EOB)
          cap_ = Const::EMPTY;
        else
          cap_ = 0;
        set_current(end_);
        got_ = Const::EOB;
        DBGLOG("Split at eof: cap = %zu txt = '%s' len = %zu", cap_, std::string(txt_, len_).c_str(), len_);
        DBGLOG("END Matcher::match()");
        return cap_;
      }
      if (cur_ == 0 && at_bob() && at_end())
      {
        cap_ = Const::EMPTY;
        got_ = Const::EOB;
      }
      else
      {
        set_current(cur_);
      }
      DBGLOG("Split: txt = '%s' len = %zu", std::string(txt_, len_).c_str(), len_);
      DBGLOG("END Matcher::match()");
      return cap_;
    }
    if (cap_ == 0)
    {
      if (method == Const::FIND && !at_end())
      {
        if (pos_ == cur_ + 1) // early fail after one non-matching char, i.e. no META executed
        {
          if (advance())
          {
            txt_ = buf_ + cur_;
            if (!pat_->one_)
              goto find;
            len_ = pat_->len_;
            txt_ = buf_ + cur_;
            set_current(cur_ + len_);
            return cap_ = 1;
          }
        }
        else if (pos_ > cur_) // we didn't fail on META alone
        {
          if (advance())
          {
            if (!pat_->one_)
              goto scan;
            len_ = pat_->len_;
            txt_ = buf_ + cur_;
            set_current(cur_ + len_);
            return cap_ = 1;
          }
        }
        txt_ = buf_ + cur_;
      }
      else
      {
        cur_ = txt_ - buf_; // no match: backup to begin of unmatched text
      }
    }
    len_ = cur_ - (txt_ - buf_);
    if (len_ == 0 && !nul)
    {
      DBGLOG("Empty or no match cur = %zu pos = %zu end = %zu", cur_, pos_, end_);
      pos_ = cur_;
      if (at_end())
      {
        set_current(cur_);
        DBGLOG("Reject empty match at EOF");
        cap_ = 0;
      }
      else if (method == Const::FIND)
      {
        DBGLOG("Reject empty match and continue?");
        set_current(++cur_); // skip one char to keep searching
        if (cap_ == 0 || !opt_.N || (!bol && c1 == '\n')) // allow FIND with "N" to match an empty line, with ^$ etc.
          goto scan;
        DBGLOG("Accept empty match");
      }
      else
      {
        set_current(cur_);
        DBGLOG("Reject empty match");
        cap_ = 0;
      }
    }
    else if (len_ == 0 && cur_ == end_)
    {
      DBGLOG("Hit end: got = %d", got_);
      if (cap_ == Const::EMPTY && !opt_.A)
        cap_ = 0;
    }
    else
    {
      set_current(cur_);
      if (len_ > 0)
      {
        if (cap_ == Const::EMPTY && !opt_.A)
        {
          DBGLOG("Ignore accept and continue: len = %zu", len_);
          len_ = 0;
          if (method != Const::MATCH)
            goto scan;
          cap_ = 0;
        }
      }
    }
    DBGLOG("Return: cap = %zu txt = '%s' len = %zu pos = %zu got = %d", cap_, std::string(txt_, len_).c_str(), len_, pos_, got_);
    DBGLOG("END match()");
    return cap_;
  }
  /// Returns true if able to advance to next possible match
  bool advance()
    /// @returns true if possible match found
    ;
#if !defined(WITH_NO_INDENT)
  /// Update indentation column counter for indent() and dedent().
  inline void newline()
  {
    mrk_ = true;
    while (ind_ + 1 < pos_)
      col_ += buf_[ind_++] == '\t' ? 1 + (~col_ & (opt_.T - 1)) : 1;
    DBGLOG("Newline with indent/dedent? col = %zu", col_);
  }
  /// Returns true if looking at indent.
  inline bool indent()
    /// @returns true if indent
  {
    newline();
    return col_ > 0 && (tab_.empty() || tab_.back() < col_);
  }
  /// Returns true if looking at dedent.
  inline bool dedent()
    /// @returns true if dedent
  {
    newline();
    return !tab_.empty() && tab_.back() > col_;
  }
  /// Returns true if nodent.
  inline bool nodent()
    /// @returns true if nodent
  {
    newline();
    return (col_ <= 0 || (!tab_.empty() && tab_.back() >= col_)) && (tab_.empty() || tab_.back() <= col_);
  }
#endif
  /// Boyer-Moore preprocessing of the given pattern pat of length len, generates bmd_ > 0 and bms_[] shifts.
  void boyer_moore_init(
      const char *pat, ///< pattern string
      size_t      len) ///< nonzero length of the pattern string, should be less than 256
    ;
  size_t            ded_;      ///< dedent count
  size_t            col_;      ///< column counter for indent matching, updated by newline(), indent(), and dedent()
  Stops             tab_;      ///< tab stops set by detecting indent margins
  std::vector<int>  lap_;      ///< lookahead position in input that heads a lookahead match (indexed by lookahead number)
  std::stack<Stops> stk_;      ///< stack to push/pop stops
  FSM               fsm_;      ///< local state for FSM code
  uint16_t          lcp_;      ///< primary least common character position in the pattern prefix or 0xffff for pure Boyer-Moore
  uint16_t          lcs_;      ///< secondary least common character position in the pattern prefix or 0xffff for pure Boyer-Moore
  size_t            bmd_;      ///< Boyer-Moore jump distance on mismatch, B-M is enabled when bmd_ > 0
  uint8_t           bms_[256]; ///< Boyer-Moore skip array
  bool              mrk_;      ///< indent \i or dedent \j in pattern found: should check and update indent stops
};

} // namespace reflex

#endif
