// C/C++ tokenizer (plain, no Unicode)

%{
#include <stdio.h>
#include <ctype.h>
#include "stack.h"

/* `line` is like that: any number of blanks then"%include"
   then any number of blanks then filename until end of line.
 */
char * extract_filename(char * line) {
  while (isspace(*line)) line++;
  line += 8; /* "%include" length */
  while (isspace(*line)) line++;
  /* The newline is not stripped because it is still used to
     print out the filename on its line. */
  return line;
}

char * strip_last_n(char * str, int n) {
  int len = strlen(str);
  str[len - n] = 0;
  return str;
}

char * strip_first_n(char * str, int n) {
  str += n;
  return str;
}

int errcnt = 0;
void errmsg_at_loc(char * tok, int line, int col, char * reason) {
  errcnt++;
  fprintf(stderr, "*** ERROR: %s at (%d;%d): ", reason, line, col+1);
  fprintf(stderr, tok);
  fprintf(stderr, "\n");
}

#define ERRMSG(_msg) {\
  char msg[] = _msg;\
  errmsg_at_loc(yytext, yylineno, columno(), msg);}

enum flag {
  TestF = 1, IfF = 2, BlockF, NestF,
  ListNF, ListDF, ListIF, ListHF, ListVF, ListArgF,
};
struct StackNode * flag_states = 0;
void flag_push (enum flag f) { push(&flag_states, f); }
void flag_pop  ()            { pop(&flag_states); }
int  flag_peek ()            { return peek(flag_states); }
int  flag_none ()            { return isEmpty(flag_states); }
int  flag_is_on(enum flag f) {
  for (
    struct StackNode * cur = flag_states;
    !isEmpty(cur);
    cur = cur->next)
    if (f == peek(cur)) return 1;
  return 0;
}

#define FLAG_MATCH(f, pop_on_ok, ok_msg, err_msg)\
{\
  if (flag_is_on(f)) { if (pop_on_ok) flag_pop(); printf(ok_msg); }\
  else ERRMSG(err_msg);\
}
%}

%option unicode
%include "cdefs.l"
%option flex
%option nodefault
%option fast
%option main
%x LambdaX LambdaArgX LambdaArgGuardX
%x ListNX ListDX ListIX ListHX ListVX ListArgX

%%

\n printf("NewLine\n");
{LWS}

/* Lambda matters begin */
[\\]                   push_state(LambdaX), printf("Lambda\n");
<LambdaX>{NAME}        push_state(LambdaArgX), printf("LambdaArg\t%s", yytext);
<LambdaArgX>{NAME}     printf("\t%s", yytext);
<LambdaArgX>{LWS}
<LambdaArgX>{LWS}?","  pop_state(), printf("\n");
<LambdaArgX>{LWS}?"{"  {
  pop_state(); pop_state();
  flag_push(BlockF);
  printf("\nBlockBegin\n"); }
<LambdaArgX>.          {
  char msg[] = "expected a name or a comma in argument list";
  errmsg_at_loc(yytext, yylineno, columno(), msg); }
<LambdaX>{LWS}?"{"     pop_state(), flag_push(BlockF), printf("BlockBegin\n");
<LambdaX>{LWS}
<LambdaX>.             {
  char msg[] = "expected a name or a beginning of a block";
  errmsg_at_loc(yytext, yylineno, columno(), msg); }
/* end */

"if"    flag_push(IfF), printf("IfBegin\n");
"fi"    FLAG_MATCH(
  IfF, 1, "IfEnd\n", "unexpected `fi` outside of `if` expresssion")
";"     FLAG_MATCH(
  IfF, 0, "IfDivider\n", "unexpected `;` outside of `if` expresssion")
"=>"    FLAG_MATCH(
  IfF, 0, "IfArm\n", "unexpected `=>` outside of `if` expresssion")

{INCLUDE}        printf("IncludeFile\t%s", extract_filename(yytext));
{COMMENT}
{NAME}{LWS}?":=" printf("AssignTo\t%s\n", strip_last_n(yytext, 2));
":"              printf("PairOperator\n");

{VALID_CHAR}     printf("CharacterLit\t%s\n", strip_first_n(yytext, 1));
{INVALID_CHAR}   ERRMSG("Invalid character literal");

{VALID_WORD}     printf("QuotedWordLit\t%s\n", strip_first_n(yytext, 1));
{INVALID_WORD}   ERRMSG("Invalid word literal");

{INTEGER}        printf("IntegerLit\t%s\n", yytext);
{FLOAT}          printf("FloatLit\t%s\n", yytext);
{STRING}         {
  char * content = strip_last_n(strip_first_n(yytext, 1), 1);
  // count of bytes!
  int len = strlen(content);
  printf("StringLit\t%d\t%s\n", len, content); }

"{"              flag_push(BlockF), printf("BlockBegin\n");
"}"              FLAG_MATCH(BlockF, 1, "BlockEnd\n", "Mismatched `}`")

"["              flag_push(NestF), printf("NestBegin\n");
"]"              FLAG_MATCH(NestF, 1, "NestEnd\n", "Mismatched `]`")

// Special names
"T"              printf("True\n");
"F"              printf("False\n");
// List prefices
"N"              push_state(ListNX);
"D"              push_state(ListDX);
"I"              push_state(ListIX);
"H"              push_state(ListHX);
"V"              push_state(ListVX);

<ListNX>"("      pop_state(), flag_push(ListNF), printf("NListBegin\n");
<ListDX>"("      pop_state(), flag_push(ListDF), printf("DListBegin\n");
<ListIX>"("      pop_state(), flag_push(ListIF), printf("IListBegin\n");
<ListHX>"("      pop_state(), flag_push(ListHF), printf("HListBegin\n");
<ListVX>"("      pop_state(), flag_push(ListVF), printf("VListBegin\n");
"("              flag_push(ListArgF), printf("ArgListBegin\n");

<ListNX>.        pop_state(); printf("Nil\n");
<ListDX>.        pop_state(); ERRMSG("Expected a list after `D`")
<ListIX>.        pop_state(); ERRMSG("Expected a list after `I`")
<ListHX>.        pop_state(); ERRMSG("Expected a list after `H`")
<ListVX>.        pop_state(); ERRMSG("Expected a list after `V`")

","{LWS}*        {
  if ((flag_peek() <= ListArgF) && (flag_peek() >= ListNF))
    printf("ListDivider\n");
  else ERRMSG("Unexpected `,`") }
")"              {
  switch (flag_peek()) {
  case ListNF:   flag_pop(); printf("NListEnd\n");   break;
  case ListDF:   flag_pop(); printf("DListEnd\n");   break;
  case ListIF:   flag_pop(); printf("IListEnd\n");   break;
  case ListHF:   flag_pop(); printf("HListEnd\n");   break;
  case ListVF:   flag_pop(); printf("VListEnd\n");   break;
  case ListArgF: flag_pop(); printf("ArgListEnd\n"); break;
  } }

{NAME}           printf("Identifier\t%s\n", yytext);
.                printf("*** ERROR '%s' at line %d\n", yytext, yylineno);
%%
