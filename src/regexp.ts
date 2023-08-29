import {
  Parser,
  SingleChar,
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
  allButChar,
  allButCharRange,
  charRange,
  all,
  orN,
  manyN,
  concat,
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
  left: RegExpType[]
  right: RegExpType[]
}
type RepetitionType = {
  type: 'repetition'
  expr: RegExpType
  min: number
  max: number
}

type RegExpType =
  | RegExpSingleCharType
  | CharacterClassType
  | ParenthesizedRegExpType
  | AlternationType
  | RepetitionType

const REPETITION_LIMITS = {
  '*': { min: 0, max: Infinity },
  '+': { min: 1, max: Infinity },
  '?': { min: 0, max: 1 },
}

const alternate = char('|')

const alternativeTerm: Parser<RegExpType[]> = input => {
  const [result, rest] = or(
    map(
      and3(many1(regExpfactor), alternate, alternativeTerm),
      ([left, _, right]) =>
        ({
          type: 'alternation',
          left,
          right,
        } as AlternationType)
    ),
    regExpfactor
  )(input) as ParserResult<RegExpType[]>

  if (isError(result)) return [result, input]

  return [result, rest]
}

const regExp = many1(alternativeTerm)

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

const characterClass = map(
  delimitedBy(char('['), and(optional(char('^')), many1(characterClassOption)), char(']')),
  ([caret, options]) =>
    ({
      type: 'characterClass',
      negated: caret === '^',
      options,
    } as CharacterClassType)
)

const parenthesizedRegExp = map(delimitedBy(openParens, regExp, closeParens), expr => ({
  type: 'parenthesizedRegExp',
  expr,
}))

const repetition = map(
  or4(
    char('*'),
    char('+'),
    char('?'),
    delimitedBy(char('{'), joinedBy(optional(natural), comma), char('}'))
  ),
  result =>
    typeof result === 'string'
      ? REPETITION_LIMITS[result as keyof typeof REPETITION_LIMITS]
      : { min: result[0] === '' ? 0 : result[0], max: result[1] === '' ? Infinity : result[1] }
)

const regExpfactor = map(
  and(or3(regExpSingleChar, characterClass, parenthesizedRegExp), optional(repetition)),
  ([expr, limits]) =>
    limits === ''
      ? expr
      : ({
          type: 'repetition',
          expr,
          ...limits,
        } as RepetitionType)
)

const evaluateRegExpPart = (part: RegExpType) => {
  switch (part.type) {
    case 'regExpSingleChar':
      return char(part.character)

    case 'parenthesizedRegExp':
      return evaluateRegExpPart(part.expr[0])

    case 'characterClass':
      return part.negated
        ? all(
            ...part.options.map((c: SingleChar | CharacterClassRangeType) =>
              typeof c === 'string' ? allButChar(c) : allButCharRange(c.from, c.to)
            )
          )
        : orN(
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
      return or(evaluateRegExpPart(part.left[0]), evaluateRegExpPart(part.right))

    default: {
      const _exhaustiveCheck: never = part
      throw new Error('Invalid regular expression type')
    }
  }
}

const regExpMatcher = (re: string): Parser<string[]> => {
  const [result, rest] = regExp(re)

  if (isError(result) || rest !== '') throw new Error('Invalid regular expression')

  return andN(...result.map(part => evaluateRegExpPart(part)))
}
