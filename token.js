const isAsciiLetter = c => /^[a-zA-Z]$/.test(c)

// Peckers return array.
// 0: value of token
// 1: rest of chars
const peckNone = chars => ["", chars.slice(1)]

const peckComment = chars =>
  [ chars.substr(1, chars.indexOf("\n")).trim()
  , chars.slice(chars.indexOf("\n" + 1))]

// peckCharacter is removed

const peckString = chars => {
  const peck = (len, rest) =>
    rest[0] == '"'
    ? [chars.substr(1, len)
      ,chars.slice(len+1)]
    : chars.startsWith("\\\"")
    ? peck(len + 2, rest.slice(2))
    : peck(len + 1, rest.slice(1))
  return peck(1, chars)
}

const peckNumber = chars => {
  let acc = "", len = 0, pointEncountered = false
  for (const c of chars) {
    if (/^[0-1]$/.test(c)) {
      acc = acc.concat(c)
      len++
    } else if ((c === ".") && !pointEncountered) {
      acc = acc.concat(c)
      len++
      pointEncountered = true
    } else {
      return [acc, chars.slice(len)]
    }
  }
}

// TODO: implement
const peckName = chars => ["", chars]

const tokenTable =
  { "{":  ["blockStart", peckNone],
    "}":  ["blockEnd",   peckNone],
    "%":  ["comment",    peckComment],
    "[":  ["arrayStart", peckNone],
    "]":  ["arrayEnd",   peckNone],
    "\n": ["newline",    peckNone],
    "\"": ["string",     peckString],
    "(":  ["parenStart", peckNone],
    ")":  ["parenEnd",   peckNone],
    ",":  ["comma",      peckNone],
    "←":  ["opAssign",   peckNone],
    "&":  ["opUnlambda", peckNone],
    "¤":  ["opLet",      peckNone],
    "λ":  ["opLambda",   peckNone],
    "⇒":  ["opThen",     peckNone],
    ";":  ["opElse",     peckNone],
  }

