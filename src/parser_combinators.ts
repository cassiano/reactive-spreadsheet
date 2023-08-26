import { RefType } from './spreadsheet_utils.ts'

const error = (msg: string) => new Error(msg)

type ParserResult<T> = [resultOrError: T | Error, rest: string]
type Parser<T> = (input: string) => ParserResult<T>
type SingleChar = string
type EmptyString = ''

export const isError = <T>(result: T | Error): result is Error => result instanceof Error

const satisfy =
  (matchFn: (char: SingleChar) => boolean): Parser<SingleChar> =>
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
  <A, B>(parserA: Parser<A>, parserB: Parser<B>): Parser<A | B> =>
  input => {
    const [resultA, restA] = parserA(input)

    return isError(resultA) ? parserB(input) : [resultA, restA]
  }
const or2 = or

const or3 = <A, B, C>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>
): Parser<A | B | C> => or(parserA, or2(parserB, parserC))

const or4 = <A, B, C, D>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>,
  parserD: Parser<D>
): Parser<A | B | C | D> => or(parserA, or3(parserB, parserC, parserD))

const or5 = <A, B, C, D, E>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>,
  parserD: Parser<D>,
  parserE: Parser<E>
): Parser<A | B | C | D | E> => or(parserA, or4(parserB, parserC, parserD, parserE))

const and = <A, B>(parserA: Parser<A>, parserB: Parser<B>): Parser<[A, B]> =>
  sequence(parserA, resultA => map(parserB, resultB => [resultA, resultB]))
const and2 = and

const and3 = <A, B, C>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>
): Parser<[A, B, C]> =>
  map(and(parserA, and2(parserB, parserC)), ([resultA, otherResults]) => [resultA, ...otherResults])

const and4 = <A, B, C, D>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>,
  parserD: Parser<D>
): Parser<[A, B, C, D]> =>
  map(and(parserA, and3(parserB, parserC, parserD)), ([resultA, otherResults]) => [
    resultA,
    ...otherResults,
  ])

const and5 = <A, B, C, D, E>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>,
  parserD: Parser<D>,
  parserE: Parser<E>
): Parser<[A, B, C, D, E]> =>
  map(and(parserA, and4(parserB, parserC, parserD, parserE)), ([resultA, otherResults]) => [
    resultA,
    ...otherResults,
  ])

type ManyOccurencesType = { minOccurences?: number; maxOccurences?: number }

const manyN =
  <A>(
    parser: Parser<A>,
    { minOccurences = 0, maxOccurences = Infinity }: ManyOccurencesType = {}
  ): Parser<A[]> =>
  input => {
    if (maxOccurences === 0) return [[], input]

    const [result, rest] = parser(input)

    if (isError(result)) return minOccurences > 0 ? [result, input] : [[], input]

    return map(
      manyN(parser, { minOccurences: minOccurences - 1, maxOccurences: maxOccurences - 1 }),
      otherResults => [result, ...otherResults]
    )(rest)
  }

const many = manyN
const many0 = many

const many1 = <A>(
  parser: Parser<A>,
  { maxOccurences = Infinity }: ManyOccurencesType = {}
): Parser<A[]> => manyN(parser, { minOccurences: 1, maxOccurences })

const many2 = <A>(
  parser: Parser<A>,
  { maxOccurences = Infinity }: ManyOccurencesType = {}
): Parser<A[]> => manyN(parser, { minOccurences: 2, maxOccurences })

const empty: Parser<EmptyString> = input => ['', input]

const optional = <A>(parser: Parser<A>): Parser<A | EmptyString> => or(parser, empty)
// const optional = <A>(parser: Parser<A>): Parser<A | EmptyString> =>
//   map(many(parser, { maxOccurences: 1 }), results => (results.length === 0 ? '' : results[0]))

const concat = (parser: Parser<string[]>): Parser<string> => map(parser, chars => chars.join(''))

const precededBy = <A>(parserBefore: Parser<unknown>, parser: Parser<A>): Parser<A> =>
  map(and(parserBefore, parser), ([_, result]) => result)

const succeededBy = <A>(parser: Parser<A>, parserAfter: Parser<unknown>): Parser<A> =>
  map(and(parser, parserAfter), ([result, _]) => result)

const delimitedBy = <A>(
  parserBefore: Parser<unknown>,
  parser: Parser<A>,
  parserAfter: Parser<unknown>
): Parser<A> => precededBy(parserBefore, succeededBy(parser, parserAfter))
// map(and3(parserBefore, parser, parserAfter), ([_, result, __]) => result)

const surroundedBy = <A>(parserBeforeAndAfter: Parser<unknown>, parser: Parser<A>): Parser<A> =>
  delimitedBy(parserBeforeAndAfter, parser, parserBeforeAndAfter)

const joinedBy = <A, B>(
  parserA: Parser<A>,
  parserInTheMiddle: Parser<unknown>,
  parserB: Parser<B>
): Parser<[A, B]> =>
  map(and(succeededBy(parserA, parserInTheMiddle), parserB), ([resultA, resultB]) => [
    resultA,
    resultB,
  ])

const char = (singleChar: SingleChar): Parser<SingleChar> => satisfy(c => c === singleChar)
const allButChar = (singleChar: SingleChar): Parser<SingleChar> => satisfy(c => c !== singleChar)
const anyChar = (): Parser<SingleChar> => satisfy(_ => true)

const charSequence =
  (seq: string): Parser<string> =>
  input =>
    input.startsWith(seq) ? [seq, input.slice(seq.length)] : [error('no match'), input]

// const letter = satisfy(char => /[a-z]/i.test(char))
const letter = satisfy(char => {
  const upcasedChar = char.toUpperCase()

  return upcasedChar >= 'A' && upcasedChar <= 'Z'
})
const letters = many1(letter)

// Decimal.
const digit = map(
  // satisfy(char => /\d/.test(char)),
  satisfy(char => char >= '0' && char <= '9'),
  digit => +digit
)
const digits = many1(digit)

// Hexadecimal.
const hexDigit: Parser<SingleChar> = satisfy(char => {
  const upcasedChar = char.toUpperCase()

  return (upcasedChar >= '0' && upcasedChar <= '9') || (upcasedChar >= 'A' && upcasedChar <= 'F')
})
const hexDigits = many1(hexDigit)
const hexNumber = concat(precededBy(charSequence('0x'), hexDigits))

// Binary.
const ZERO = '0'
const ONE = '1'
const zero = char(ZERO)
const one = char(ONE)
const bit = or(zero, one)
const binaryDigits = many1(bit)
const binaryNumber = concat(precededBy(charSequence('0b'), binaryDigits))

const DOUBLE_QUOTE = '"'
const SINGLE_QUOTE = "'"
const BACK_TICK = '`'
const UNDERSCORE = '_'
const PLUS_SIGN = '+'
const MINUS_SIGN = '-'
const PERIOD = '.'
const EQUALS = '='

const doubleQuote = char(DOUBLE_QUOTE)
const singleQuote = char(SINGLE_QUOTE)
const backTick = char(BACK_TICK)
const underscore = char(UNDERSCORE)
const plus = char(PLUS_SIGN)
const minus = char(MINUS_SIGN)
const period = char(PERIOD)
const equals = char(EQUALS)

const string = concat(
  or3(
    surroundedBy(doubleQuote, many(allButChar(DOUBLE_QUOTE))),
    surroundedBy(singleQuote, many(allButChar(SINGLE_QUOTE))),
    surroundedBy(backTick, many(allButChar(BACK_TICK)))
  )
)

const wordChar = map(or3(letter, digit, underscore), res => res.toString())
const word = concat(many1(wordChar))
const identifier = word

const sign = or(plus, minus)

const BASE_10 = 10

const natural = map(precededBy(optional(plus), digits), digs =>
  digs.reduce((acc, dig, i) => acc + dig * BASE_10 ** (digs.length - (i + 1)), 0)
)

const integer = map(
  and(optional(sign), natural),
  ([signChar, nat]) => (signChar === MINUS_SIGN ? -1 : 1) * nat
)

const naturalGreaterThanZero: Parser<number> = input => {
  const [result, rest] = natural(input)

  if (isError(result)) return [result, input]
  if (!(result > 0)) return [error(`Number must be > 0, but was ${result}`), input]

  return [result, rest]
}

const float = map(
  and(integer, precededBy(period, natural)),
  ([int, nat]) =>
    int + (nat === 0 ? 0 : (Math.sign(int) * nat) / BASE_10 ** Math.trunc(Math.log10(nat) + 1))
)

const numeric = or(float, integer)

export const ADDED_TO = '+'
export const SUBTRACTED_FROM = '-'
export const MULTIPLIED_BY = '*'
export const DIVIDED_BY = '/'
export const RAISED_TO = '^'
export const RAISED_TO_ALT = '**'
export const OPEN_PARENS = '('
export const CLOSE_PARENS = ')'

const add = char(ADDED_TO)
const subtract = char(SUBTRACTED_FROM)
const multiply = char(MULTIPLIED_BY)
const divided = char(DIVIDED_BY)
const exponentiate = or(char(RAISED_TO), charSequence(RAISED_TO_ALT))
const openParens = char(OPEN_PARENS)
const closeParens = char(CLOSE_PARENS)

export type OperatorType =
  | typeof ADDED_TO
  | typeof SUBTRACTED_FROM
  | typeof MULTIPLIED_BY
  | typeof DIVIDED_BY
  | typeof RAISED_TO
  | typeof RAISED_TO_ALT

export type RangeType = { type: 'range'; from: RefType; to: RefType }

type BinaryOperationType = {
  type: 'binaryOperation'
  left: ExpressionType
  operator: OperatorType
  right: ExpressionType
}
type NumericType = { type: 'numeric'; value: number }
type ReferenceType = { type: 'reference'; ref: RefType }
type ParenthesizedExpressionType = { type: 'parenthesizedExpression'; expr: ExpressionType }
type AggregationFnCallType = {
  type: 'aggregationFnCall'
  fnName: string
  parameters: (RangeType | ExpressionType)[]
}

export type ExpressionType =
  | BinaryOperationType
  | NumericType
  | ReferenceType
  | ParenthesizedExpressionType
  | AggregationFnCallType

// https://stackoverflow.com/questions/2969561/how-to-parse-mathematical-expressions-involving-parentheses
//
// const additionSubtractionTerm: Parser<ExpressionType> = input =>
//   or(and3(multiplicationDivisionTerm, or(plus, minus), expression), multiplicationDivisionTerm)(input)
// const multiplicationDivisionTerm: Parser<ExpressionType> = input =>
//   or(and3(exponentiationTerm, or(times, dividedBy), multiplicationDivisionTerm), exponentiationTerm)(input)
// const exponentiationTerm: Parser<ExpressionType> = input =>
//   or(and3(factor, toThePowerOf, exponentiationTerm), factor)(input)
// const factor = or(operand, delimitedBy(openParens, expression, closeParens))
// const expression = additionSubtractionTerm

const ref = concat(
  map(and(letters, naturalGreaterThanZero), ([col, row]) => [...col, row.toString()])
)

const NUMBER_1: NumericType = { type: 'numeric', value: 1 }
const NUMBER_MINUS_1: NumericType = { type: 'numeric', value: -1 }

const createBinaryOperation = (
  left: ExpressionType,
  operator: OperatorType,
  right: ExpressionType
) =>
  ({
    type: 'binaryOperation',
    left,
    operator,
    right,
  } as BinaryOperationType)

const mapToBinaryOperation = (parser: Parser<[ExpressionType, OperatorType, ExpressionType]>) =>
  map(parser, ([left, operator, right]) => createBinaryOperation(left, operator, right))

const additiveTerm: Parser<ExpressionType> = input => {
  const [result, rest] = or(
    mapToBinaryOperation(
      and3(multiplicativeTerm, or(add, subtract) as Parser<OperatorType>, additiveTerm)
    ),
    multiplicativeTerm
  )(input) as ParserResult<ExpressionType>

  if (isError(result)) return [result, input]

  // Given that:
  //
  // 1) '+' is both left-associative (i.e. can be evaluated from left to right) and right-associative
  //    (from right to left), so: a + b + c = a + (b + c) = (a + b) + c
  // 2) '-' is only left-associative, so: a - b - c = (a - b) - c ≠ a - (b - c)
  // 3) our parser is right-recursive (so will effectively evaluate from right to left)
  //
  // replace `a - b` by `a + (-b)`.
  if (
    result.type === 'binaryOperation' &&
    result.operator === SUBTRACTED_FROM &&
    (result.right.type === 'binaryOperation' || result.right.type === 'parenthesizedExpression')
  ) {
    result.operator = ADDED_TO

    if (result.right.type === 'parenthesizedExpression') {
      result.right = createBinaryOperation(NUMBER_MINUS_1, MULTIPLIED_BY, result.right.expr)
    } else {
      result.right.left = createBinaryOperation(NUMBER_MINUS_1, MULTIPLIED_BY, result.right.left)
    }
  }

  return [result, rest]
}

const expression = additiveTerm

const multiplicativeTerm: Parser<ExpressionType> = input => {
  const [result, rest] = or(
    mapToBinaryOperation(
      and3(exponentialTerm, or(multiply, divided) as Parser<OperatorType>, multiplicativeTerm)
    ),
    exponentialTerm
  )(input) as ParserResult<ExpressionType>

  if (isError(result)) return [result, input]

  // Given that:
  //
  // 1) '*' is both left-associative (i.e. can be evaluated from left to right) and right-associative
  //    (from right to left), so: a * b * c = a * (b * c) = (a * b) * c
  // 2) '/' is only left-associative, so: a / b / c = (a / b) / c ≠ a / (b / c)
  // 3) our parser is right-recursive (so will effectively evaluate from right to left)
  //
  // replace `a / b` by `a * (1 / b)`.
  if (
    result.type === 'binaryOperation' &&
    result.operator === DIVIDED_BY &&
    (result.right.type === 'binaryOperation' || result.right.type === 'parenthesizedExpression')
  ) {
    result.operator = MULTIPLIED_BY

    if (result.right.type === 'parenthesizedExpression') {
      result.right = createBinaryOperation(NUMBER_1, DIVIDED_BY, result.right.expr)
    } else {
      result.right.left = createBinaryOperation(NUMBER_1, DIVIDED_BY, result.right.left)
    }
  }

  return [result, rest]
}

const exponentialTerm: Parser<ExpressionType> = input =>
  or(
    mapToBinaryOperation(
      and3(factor as Parser<ExpressionType>, exponentiate as Parser<OperatorType>, exponentialTerm)
    ),
    factor
  )(input) as ParserResult<ExpressionType>

const optionallySigned = <A extends ExpressionType>(parser: Parser<A>) =>
  map(and(optional(sign), parser), ([signChar, result]) =>
    signChar === MINUS_SIGN ? createBinaryOperation(NUMBER_MINUS_1, MULTIPLIED_BY, result) : result
  )

const operand = or(
  map(numeric, value => ({ type: 'numeric', value })),
  optionallySigned(map(ref, ref => ({ type: 'reference', ref })))
)

const parenthesizedExpression = optionallySigned(
  map(delimitedBy(openParens, expression, closeParens), expr => ({
    type: 'parenthesizedExpression',
    expr,
  }))
)

const colon = char(':')
const comma = char(',')
const range: Parser<RangeType> = map(joinedBy(ref, colon, ref), ([from, to]) => ({
  type: 'range',
  from,
  to,
}))

const fnParameter = or(range, expression)

const aggregationFnCall = map(
  and(
    identifier,
    delimitedBy(
      openParens,
      and(optional(fnParameter), many(precededBy(comma, fnParameter))),
      closeParens
    )
  ),
  ([fnName, [param1, otherParams]]) =>
    ({
      type: 'aggregationFnCall',
      fnName,
      parameters: param1 === '' ? [] : [param1, ...otherParams],
    } as AggregationFnCallType)
)

const factor = or3(operand, parenthesizedExpression, aggregationFnCall)

export const formula = precededBy(equals, expression)
