const error = (msg: string) => new Error(msg)

type ParserResult<T> = [resultOrError: T | Error, rest: string]
type Parser<T> = (input: string) => ParserResult<T>
type SingleChar = string
type EmptyString = ''

export const isError = <T>(result: T | Error): result is Error => result instanceof Error

const satisfy =
  (matchFn: (char: SingleChar) => boolean): Parser<string> =>
  input =>
    input.length > 0 && matchFn(input[0]) ? [input[0], input.slice(1)] : [error('no match'), input]

const map =
  <A, B>(parser: Parser<A>, fn: (value: A) => B): Parser<B> =>
  input => {
    const [result, rest] = parser(input)

    return isError(result) ? [result, input] : [fn(result), rest]
  }

const sequence =
  <A, B>(parser: Parser<A>, fn: (value: A) => Parser<B>): Parser<B> =>
  input => {
    const [result, rest] = parser(input)

    return isError(result) ? [result, input] : fn(result)(rest)
  }

const or =
  <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<A | B> =>
  input => {
    const [result1, rest1] = parser1(input)

    return !isError(result1) ? [result1, rest1] : parser2(input)
  }
const or2 = or

const or3 = <A, B, C>(parser1: Parser<A>, parser2: Parser<B>, parser3: Parser<C>): Parser<A | B | C> =>
  or(parser1, or2(parser2, parser3))

const or4 = <A, B, C, D>(
  parser1: Parser<A>,
  parser2: Parser<B>,
  parser3: Parser<C>,
  parser4: Parser<D>
): Parser<A | B | C | D> => or(parser1, or3(parser2, parser3, parser4))

const or5 = <A, B, C, D, E>(
  parser1: Parser<A>,
  parser2: Parser<B>,
  parser3: Parser<C>,
  parser4: Parser<D>,
  parser5: Parser<E>
): Parser<A | B | C | D | E> => or(parser1, or4(parser2, parser3, parser4, parser5))

const and = <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<[A, B]> =>
  sequence(parser1, result1 => map(parser2, result2 => [result1, result2]))
const and2 = and

const and3 = <A, B, C>(parser1: Parser<A>, parser2: Parser<B>, parser3: Parser<C>): Parser<[A, B, C]> =>
  map(and(parser1, and2(parser2, parser3)), ([result1, results]) => [result1, ...results])

const and4 = <A, B, C, D>(
  parser1: Parser<A>,
  parser2: Parser<B>,
  parser3: Parser<C>,
  parser4: Parser<D>
): Parser<[A, B, C, D]> =>
  map(and(parser1, and3(parser2, parser3, parser4)), ([result1, results]) => [result1, ...results])

const and5 = <A, B, C, D, E>(
  parser1: Parser<A>,
  parser2: Parser<B>,
  parser3: Parser<C>,
  parser4: Parser<D>,
  parser5: Parser<E>
): Parser<[A, B, C, D, E]> =>
  map(and(parser1, and4(parser2, parser3, parser4, parser5)), ([result1, results]) => [result1, ...results])

type ManyOccurencesType = { minOccurences?: number; maxOccurences?: number }

const manyN =
  <A>(parser: Parser<A>, { minOccurences = 0, maxOccurences = Infinity }: ManyOccurencesType = {}): Parser<A[]> =>
  input => {
    const [result, rest] = parser(input)

    if (isError(result)) return minOccurences > 0 ? [result, input] : [[], input]
    if (maxOccurences === 0) return [[], input]

    return map(manyN(parser, { minOccurences: minOccurences - 1, maxOccurences: maxOccurences - 1 }), results => [
      result,
      ...results,
    ])(rest)
  }

const many = manyN
const many0 = many
const many1 = <A>(parser: Parser<A>, { maxOccurences = Infinity }: ManyOccurencesType = {}): Parser<A[]> =>
  manyN(parser, { minOccurences: 1, maxOccurences })
const many2 = <A>(parser: Parser<A>, { maxOccurences = Infinity }: ManyOccurencesType = {}): Parser<A[]> =>
  manyN(parser, { minOccurences: 2, maxOccurences })

const empty: Parser<EmptyString> = input => ['', input]

const optional = <T>(parser: Parser<T>): Parser<EmptyString | T> => or(parser, empty)
// const optional = <T>(parser: Parser<T>): Parser<EmptyString | T> =>
//   map(many(parser, { maxOccurences: 1 }), results => (results.length === 0 ? '' : results[0]))

const concat = (parser: Parser<string[]>): Parser<string> => map(parser, chars => chars.join(''))

const precededBy = <A, B>(parserBefore: Parser<A>, parser: Parser<B>): Parser<B> =>
  map(and(parserBefore, parser), ([_, result]) => result)

const succeededBy = <A, B>(parser: Parser<A>, parserAfter: Parser<B>): Parser<A> =>
  map(and(parser, parserAfter), ([result, _]) => result)

const delimitedBy = <A, B, C>(parserBefore: Parser<A>, parser: Parser<B>, parserAfter: Parser<C>): Parser<B> =>
  precededBy(parserBefore, succeededBy(parser, parserAfter))

const char = (singleChar: SingleChar): Parser<SingleChar> => satisfy(c => c === singleChar)
const allButChar = (singleChar: SingleChar): Parser<SingleChar> => satisfy(c => c !== singleChar)
const anyChar = (): Parser<SingleChar> => satisfy(_ => true)

const word =
  (w: string): Parser<string> =>
  (input: string) =>
    input.startsWith(w) ? [w, input.slice(w.length)] : [error('no match'), input]

// const letter = satisfy(char => /[a-z]/i.test(char))
const letter = satisfy(char => char.toUpperCase() >= 'A' && char.toUpperCase() <= 'Z')
const letters = many1(letter)

// Decimal.
const digit = map(
  // satisfy(char => /\d/.test(char)),
  satisfy(char => char >= '0' && char <= '9'),
  digit => +digit
)
const digits = many1(digit)

// Hexadecimal.
const hexDigit: Parser<SingleChar> = satisfy(
  char => (char >= '0' && char <= '9') || (char.toUpperCase() >= 'A' && char.toUpperCase() <= 'F')
)
const hexDigits = many1(hexDigit)
// const hexNumber = map(and3(char('0'), char('x'), hexDigits), ([_0, _x, digs]) => digs.join(''))
const hexNumber = concat(precededBy(word('0x'), hexDigits))

const DOUBLE_QUOTE = '"'
const SINGLE_QUOTE = "'"
const BACK_TICK = '`'

const doubleQuote = char(DOUBLE_QUOTE)
const singleQuote = char(SINGLE_QUOTE)
const backTick = char(BACK_TICK)
const underscore = char('_')
const plus = char('+')
const minus = char('-')
const period = char('.')
const equals = char('=')

const string = concat(
  or3(
    precededBy(doubleQuote, succeededBy(many(allButChar(DOUBLE_QUOTE)), doubleQuote)),
    precededBy(singleQuote, succeededBy(many(allButChar(SINGLE_QUOTE)), singleQuote)),
    precededBy(backTick, succeededBy(many(allButChar(BACK_TICK)), backTick))
  )
)

const identifierChar = map(or3(letter, digit, underscore), res => res.toString())
const identifier = concat(many1(identifierChar))

const ref = map(and(letters, digits), result => result.flat(2).join(''))

const sign = or(plus, minus)

const natural = map(precededBy(optional(plus), digits), digs =>
  digs.reduce((acc, dig, i) => acc + dig * 10 ** (digs.length - (i + 1)), 0)
)

const integer = map(and(optional(sign), natural), ([signChar, nat]) => (signChar === '-' ? -1 : +1) * nat)

const naturalGreaterThanZero: Parser<number> = input => {
  const [result, rest] = natural(input)

  if (isError(result)) return [result, input]
  if (!(result > 0)) return [error(`Number must be > 0, but was ${result}`), input]

  return [result, rest]
}

const float = map(
  and(integer, precededBy(period, natural)),
  ([int, nat]) => int + (nat === 0 ? 0 : (Math.sign(int) * nat) / 10 ** Math.trunc(Math.log10(nat) + 1))
)

const numeric = or(float, integer)

const times = char('*')
const dividedBy = char('/')
const openParens = char('(')
const closeParens = char(')')

export type OperatorType = '+' | '-' | '*' | '/'
export type ExpressionType =
  | (string | number)
  | [ExpressionType, OperatorType, ExpressionType]
  | ['(', ExpressionType, ')']

// const expression: Parser<ExpressionType> = (input: string) => or(and3(term, or(plus, minus), expression), term)(input)
// const term: Parser<ExpressionType> = (input: string) => or(and3(factor, or(times, dividedBy), term), factor)(input)
// const factor = or(operand, succeededBy(precededBy(openParens, expression), closeParens))

const operand = or(
  numeric,
  map(and(optional(sign), ref), ([signChar, reference]) => (signChar === '-' ? [-1, '*', reference] : reference))
)

const expression: Parser<ExpressionType> = (input: string) => {
  const [result, rest] = or(and3(term, or(plus, minus), expression), term)(input)

  if (isError(result)) return [result, input]

  // Replace `a - b` by `a + (-b)`.
  if (Array.isArray(result) && result[1] === '-' && Array.isArray(result[2])) {
    result[1] = '+'
    result[2][0] = [-1, '*', result[2][0]]
  }

  return [result, rest]
}

const term: Parser<ExpressionType> = (input: string) => {
  const [result, rest] = or(and3(factor, or(times, dividedBy), term), factor)(input)

  if (isError(result)) return [result, input]

  // Replace `a / b` by `a * (1 / b)`, but only if `b` is not a parenthesised expression.
  if (
    Array.isArray(result) &&
    result[1] === '/' &&
    Array.isArray(result[2]) &&
    !(result[2][0] === '(' && result[2][2] === ')')
  ) {
    result[1] = '*'
    result[2][0] = [1, '/', result[2][0]]
  }

  return [result, rest]
}

const factor = or(operand, and3(openParens, expression, closeParens))

export const formula = precededBy(equals, expression)
