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
  or4,
  satisfy,
  isError,
  or3,
  and3,
  ParserResult,
  andN,
  charRange,
  orN,
  manyN,
  concat,
  none,
  anyChar,
  PERIOD,
  EMPTY_STRING,
  SingleChar,
} from './parser_combinators.ts'

type CharacterClassRangeType = { from: SingleChar; to: SingleChar }

type RegExpSingleCharType = {
  type: 'regExpSingleChar'
  character: SingleChar
}
type CharacterClassType = {
  type: 'characterClass'
  negated: boolean
  options: [SingleChar | CharacterClassRangeType]
}
type ParenthesizedRegExpType = {
  type: 'parenthesizedRegExp'
  expr: RegExpType
}
type AlternationType = {
  type: 'alternation'
  left: RegExpType
  right: RegExpType
}
type RepetitionType = {
  type: 'repetition'
  expr: RegExpTypePart
  min: number
  max: number
}

type RegExpTypePart =
  | RegExpSingleCharType
  | CharacterClassType
  | ParenthesizedRegExpType
  | AlternationType
  | RepetitionType

type RegExpType = RegExpTypePart[]

const REPETITION_LIMITS = {
  '*': { min: 0, max: Infinity },
  '+': { min: 1, max: Infinity },
  '?': { min: 0, max: 1 },
}

const alternate = char('|')

const alternativeTerm: Parser<RegExpTypePart> = input => {
  const [result, rest] = or(
    map(
      and3(many1(regExpfactor), alternate, many1(alternativeTerm)),
      ([left, _, right]) =>
        ({
          type: 'alternation',
          left,
          right,
        } as AlternationType)
    ),
    regExpfactor
  )(input) as ParserResult<RegExpTypePart>

  if (isError(result)) return [result, input]

  return [result, rest]
}

const regExp: Parser<RegExpType> = many1(alternativeTerm)

const regExpSingleChar = map(
  satisfy(c => !'*+?|{}[]()'.includes(c)),
  character => ({ type: 'regExpSingleChar', character } as RegExpSingleCharType)
)

const characterClassChar = satisfy(c => c !== ']')

const characterClassOption = or(
  map(
    or(joinedBy(letter, char('-')), joinedBy(digit, char('-'))),
    range =>
      ({
        from: range[0].toString(),
        to: range[1].toString(),
      } as CharacterClassRangeType)
  ),
  characterClassChar
)

const characterClass: Parser<CharacterClassType> = map(
  delimitedBy(char('['), and(optional(char('^')), many1(characterClassOption)), char(']')),
  ([caret, options]) =>
    ({
      type: 'characterClass',
      negated: caret === '^',
      options,
    } as CharacterClassType)
)

const parenthesizedRegExp: Parser<ParenthesizedRegExpType> = map(
  delimitedBy(openParens, regExp, closeParens),
  expr => ({
    type: 'parenthesizedRegExp',
    expr,
  })
)

const repetition: Parser<Pick<RepetitionType, 'min' | 'max'>> = map(
  or4(
    char('*'),
    char('+'),
    char('?'),
    delimitedBy(char('{'), or(joinedBy(optional(natural), comma), natural), char('}'))
  ),
  result =>
    typeof result === 'string'
      ? REPETITION_LIMITS[result as keyof typeof REPETITION_LIMITS]
      : typeof result === 'number'
      ? { min: result, max: result }
      : {
          min: result[0] === EMPTY_STRING ? 0 : result[0],
          max: result[1] === EMPTY_STRING ? Infinity : result[1],
        }
)

const regExpfactor: Parser<RegExpTypePart> = map(
  and(or3(regExpSingleChar, characterClass, parenthesizedRegExp), optional(repetition)),
  ([expr, limits]) =>
    limits === EMPTY_STRING
      ? expr
      : ({
          type: 'repetition',
          expr,
          ...limits,
        } as RepetitionType)
)

const evaluateRegExpPart = (part: RegExpTypePart): Parser<string> => {
  switch (part.type) {
    case 'regExpSingleChar':
      return part.character === PERIOD ? anyChar() : char(part.character)

    case 'parenthesizedRegExp':
      return concat(andN(...part.expr.map(evaluateRegExpPart)))

    case 'characterClass':
      return part.negated
        ? none({ charsToConsume: 1 })(
            ...part.options.map((c: SingleChar | CharacterClassRangeType) =>
              typeof c === 'string' ? char(c) : charRange(c.from, c.to)
            )
          )
        : // ? all(
          //     ...part.options.map((c: SingleChar | CharacterClassRangeType) =>
          //       typeof c === 'string' ? allButChar(c) : allButCharRange(c.from, c.to)
          //     )
          //   )
          orN(
            ...part.options.map((c: SingleChar | CharacterClassRangeType) =>
              typeof c === 'string' ? char(c) : charRange(c.from, c.to)
            )
          )

    case 'repetition':
      return concat(
        manyN(evaluateRegExpPart(part.expr), {
          minOccurences: part.min,
          maxOccurences: part.max,
        })
      )

    case 'alternation':
      return concat(
        or(andN(...part.left.map(evaluateRegExpPart)), andN(...part.right.map(evaluateRegExpPart)))
      )

    default: {
      const _exhaustiveCheck: never = part
      throw new Error('Invalid regular expression type')
    }
  }
}

const CHARACTER_CLASS_ABBREVIATIONS: { [index: SingleChar]: string } = {
  d: '[0-9]',
  h: '[0-9a-fA-F]',
  w: '[0-9a-zA-Z_]',
  s: '[ \t\r\n\f]',
  r: '[\r\n]',
}

const replaceCharacterClassAbbreviations = (re: string) => {
  Object.entries(CHARACTER_CLASS_ABBREVIATIONS).forEach(([k, v]) => {
    // Create regular abbreviations (\d, \h, \w etc, as well as /d, /h, /w etc).
    re = re.replaceAll(`\\${k}`, v).replaceAll(`/${k}`, v)

    // Create "negated" upper-cased abbreviations (\D, \H, \W etc, as well as /D, /H, /W etc).
    k = k.toUpperCase()
    v = v.slice(0, 1) + '^' + v.slice(1)
    re = re.replaceAll(`\\${k}`, v).replaceAll(`/${k}`, v)
  })

  return re
}

const buildRegExp = (regExpAsString: string): RegExpType => {
  const [result, rest] = regExp(replaceCharacterClassAbbreviations(regExpAsString))

  if (isError(result) || rest !== EMPTY_STRING) throw new Error('Invalid regular expression')

  return result
}

const regExpParser = (regExpAsString: string): Parser<string> => {
  const re = buildRegExp(regExpAsString)

  return concat(andN(...re.map(part => evaluateRegExpPart(part))))
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

const regExpMatcher =
  (regExpAsString: string): Parser<string> =>
  input => {
    const parser = regExpParser(regExpAsString)

    return match(parser, input)
  }

const scan =
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

const print = (value: object) => console.log(Deno.inspect(value, { depth: 999, colors: true }))

const showRegExp = (regExpAsString: string) => print(buildRegExp(regExpAsString))
