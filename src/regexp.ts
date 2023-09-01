import {
  Parser,
  and,
  char,
  closeParens,
  comma,
  delimitedBy,
  digit,
  joinedBy,
  letter,
  many1,
  map,
  natural,
  openParens,
  optional,
  or,
  isError,
  or3,
  ParserResult,
  andN,
  charRange,
  orN,
  manyN,
  concat,
  anyChar,
  PERIOD,
  EMPTY_STRING,
  SingleChar,
  allButChar,
  many,
  succeededBy,
  allButCharSet,
  none1,
} from './parser_combinators.ts'

type CharacterClassRangeType = { from: SingleChar; to: SingleChar }

type RepetitionLimitsType = {
  min: number
  max: number
}

type SingleCharType = {
  type: 'singleChar'
  character: SingleChar
}
type CharacterClassType = {
  type: 'characterClass'
  negated: boolean
  options: (SingleChar | CharacterClassRangeType)[]
}
type ParenthesizedType = {
  type: 'parenthesized'
  expr: RegExpType
}
type AlternationType = {
  type: 'alternation'
  left: RegExpType
  right: RegExpType
}
type RepetitionType = {
  type: 'repetition'
  expr: SingleCharType | CharacterClassType | ParenthesizedType
  limits: RepetitionLimitsType
}

type RegExpTypePart =
  | SingleCharType
  | CharacterClassType
  | ParenthesizedType
  | RepetitionType
  | AlternationType

type RegExpType = RegExpTypePart[]

const QUANTIFIERS: { [index: SingleChar]: RepetitionLimitsType } = {
  '*': { min: 0, max: Infinity },
  '+': { min: 1, max: Infinity },
  '?': { min: 0, max: 1 },
}

const alternate = char('|')

const alternativeTerm: Parser<RegExpTypePart> = input =>
  or(
    map(
      and(succeededBy(many1(regExpfactor), alternate), many(alternativeTerm)),
      ([left, right]) => ({
        type: 'alternation' as const,
        left,
        right,
      })
    ),
    regExpfactor
  )(input)

const regExp: Parser<RegExpType> = many1(alternativeTerm)

const regExpSingleChar: Parser<SingleCharType> = map(
  allButCharSet('*+?|{}[]()$^'),
  character => ({ type: 'singleChar', character } as SingleCharType)
)

const dash = char('-')
const characterClassChar = allButChar(']')

const characterClassOption: Parser<string | CharacterClassRangeType> = or(
  map(or(joinedBy(letter, dash), joinedBy(digit, dash)), range => ({
    from: range[0].toString(),
    to: range[1].toString(),
  })),
  characterClassChar
)

const CARET = '^'

const characterClass: Parser<CharacterClassType> = map(
  delimitedBy(char('['), and(optional(char(CARET)), many1(characterClassOption)), char(']')),
  ([caret, options]) => ({
    type: 'characterClass',
    negated: caret === CARET,
    options,
  })
)

const parenthesizedRegExp: Parser<ParenthesizedType> = map(
  delimitedBy(openParens, regExp, closeParens),
  expr => ({
    type: 'parenthesized',
    expr,
  })
)

const quantifier: Parser<RepetitionLimitsType> = map(
  or(
    orN(Object.keys(QUANTIFIERS).map(quant => char(quant))),
    delimitedBy(char('{'), or(joinedBy(optional(natural), comma), natural), char('}'))
  ),
  result =>
    typeof result === 'string'
      ? QUANTIFIERS[result as keyof typeof QUANTIFIERS]
      : typeof result === 'number'
      ? { min: result, max: result }
      : {
          min: result[0] === EMPTY_STRING ? 0 : result[0],
          max: result[1] === EMPTY_STRING ? Infinity : result[1],
        }
)

const regExpfactor: Parser<RegExpTypePart> = map(
  and(or3(regExpSingleChar, characterClass, parenthesizedRegExp), optional(quantifier)),
  ([expr, limits]) =>
    limits === EMPTY_STRING
      ? expr
      : {
          type: 'repetition',
          expr,
          limits,
        }
)

const evaluateRegExpPart = (part: RegExpTypePart): Parser<string> => {
  switch (part.type) {
    case 'singleChar':
      return part.character === PERIOD ? anyChar() : char(part.character)

    case 'parenthesized':
      return concat(andN(part.expr.map(evaluateRegExpPart)))

    case 'characterClass': {
      const optionsParser = part.options.map(option =>
        typeof option === 'string' ? char(option) : charRange(option.from, option.to)
      )

      // Negated alternative:
      //
      // all(
      //   part.options.map((c: SingleChar | CharacterClassRangeType) =>
      //     typeof c === 'string' ? allButChar(c) : allButCharRange(c.from, c.to)
      //   )
      // )

      return !part.negated ? orN(optionsParser) : none1(optionsParser)
    }

    case 'repetition':
      return concat(manyN(evaluateRegExpPart(part.expr), part.limits))

    case 'alternation':
      return concat(
        or(andN(part.left.map(evaluateRegExpPart)), andN(part.right.map(evaluateRegExpPart)))
      )

    default: {
      const _exhaustiveCheck: never = part
      throw new Error('Invalid regular expression type')
    }
  }
}

const CHARACTER_CLASS_ABBREVIATIONS: { [index: SingleChar]: string } = {
  d: '[0-9]',
  b: '[0-1]',
  h: '[0-9a-fA-F]',
  w: '[0-9a-zA-Z_]',
  s: '[ \t\r\n\f]',
  r: '[\r\n]',
}

const replaceCharacterClassAbbreviations = (re: string): string => {
  Object.entries(CHARACTER_CLASS_ABBREVIATIONS).forEach(([abbrev, characterClass]) => {
    // Create regular abbreviations (\d, \h, \w etc, as well as /d, /h, /w etc).
    re = re.replaceAll(`\\${abbrev}`, characterClass).replaceAll(`/${abbrev}`, characterClass)

    // Create "negated" upper-cased abbreviations (\D, \H, \W etc, as well as /D, /H, /W etc).
    abbrev = abbrev.toUpperCase()
    characterClass = characterClass.slice(0, 1) + '^' + characterClass.slice(1)
    re = re.replaceAll(`\\${abbrev}`, characterClass).replaceAll(`/${abbrev}`, characterClass)
  })

  return re
}

export const buildRegExp = (regExpAsString: string): RegExpType => {
  const [result, rest] = regExp(replaceCharacterClassAbbreviations(regExpAsString))

  if (isError(result) || rest !== EMPTY_STRING) throw new Error('Invalid regular expression')

  return result
}

const regExpParser = (regExpAsString: string): Parser<string> => {
  const re = buildRegExp(regExpAsString)

  return concat(andN(re.map(evaluateRegExpPart)))
}

const match = (parser: Parser<string>, input: string): ParserResult<string> => {
  let result!: string | Error
  let rest!: string

  // Try to match the regular expression from left to right.
  for (let i = 0; i < input.length; i++) {
    ;[result, rest] = parser(input.slice(i))

    if (!isError(result)) return [result, rest]
  }

  return [result, rest]
}

export const regExpMatcher =
  (regExpAsString: string): Parser<string> =>
  input => {
    const parser = regExpParser(regExpAsString)

    return match(parser, input)
  }

export const scan =
  (regExpAsString: string) =>
  (input: string): string[] => {
    let stop = false
    let rest = input
    const matches = []

    const parser = regExpParser(regExpAsString)

    while (!stop) {
      const [result, remaining] = match(parser, rest)

      if (!isError(result)) {
        matches.push(result)

        rest = remaining
      }

      if (isError(result) || remaining === EMPTY_STRING) stop = true
    }

    return matches.flat()
  }

declare const Deno: { inspect: (...args: unknown[]) => void }

export const print = (value: object) =>
  console.log(Deno.inspect(value, { depth: 999, colors: true }))

export const showRegExp = (regExpAsString: string) => print(buildRegExp(regExpAsString))
