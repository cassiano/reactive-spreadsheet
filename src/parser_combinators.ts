import { RefType } from './spreadsheet_utils.ts'

const error = (msg: string) => new Error(msg)

const EMPTY_STRING = ''

type ParserResult<T> = [resultOrError: T | Error, rest: string]
type Parser<T> = (input: string) => ParserResult<T>
export type SingleChar = string
type EmptyString = typeof EMPTY_STRING

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

const empty: Parser<EmptyString> = input => [EMPTY_STRING, input]

const optional = <A>(parser: Parser<A>): Parser<A | EmptyString> => or(parser, empty)
// const optional = <A>(parser: Parser<A>): Parser<A | EmptyString> =>
//   map(many(parser, { maxOccurences: 1 }), results => (results.length === 0 ? EMPTY_STRING : results[0]))

const concat = (parser: Parser<string[]>): Parser<string> =>
  map(parser, chars => chars.join(EMPTY_STRING))

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

const joinedBy = <A>(parser: Parser<A>, parserInTheMiddle: Parser<unknown>): Parser<[A, A]> =>
  map(and(succeededBy(parser, parserInTheMiddle), parser), ([result1, result2]) => [
    result1,
    result2,
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

export type HexDigitType =
  | '0'
  | '1'
  | '2'
  | '3'
  | '4'
  | '5'
  | '6'
  | '7'
  | '8'
  | '9'
  | 'A'
  | 'B'
  | 'C'
  | 'D'
  | 'E'
  | 'F'

// Hexadecimal.
const hexDigit = satisfy(char => {
  const upcasedChar = char.toUpperCase()

  return (upcasedChar >= '0' && upcasedChar <= '9') || (upcasedChar >= 'A' && upcasedChar <= 'F')
}) as Parser<HexDigitType>
const hexDigits = many1(hexDigit)
const hexNumber = concat(and(charSequence('0x'), concat(hexDigits)))

type BitType = '0' | '1'

// Binary.
const ZERO = '0'
const ONE = '1'
const zero = char(ZERO)
const one = char(ONE)
const bit = or(zero, one) as Parser<BitType>
const binaryDigits = many1(bit)
const binaryNumber = concat(and(charSequence('0b'), concat(binaryDigits)))

const SPACE = ' '

const spaced = <A>(parser: Parser<A>): Parser<A> => surroundedBy(many(char(SPACE)), parser)

const DOUBLE_QUOTE = '"'
const SINGLE_QUOTE = "'"
const BACK_TICK = '`'
const UNDERSCORE = '_'
const PLUS_SIGN = '+'
const MINUS_SIGN = '-'
const PERIOD = '.'

const doubleQuote = char(DOUBLE_QUOTE)
const singleQuote = char(SINGLE_QUOTE)
const backTick = char(BACK_TICK)
const underscore = char(UNDERSCORE)
const plus = char(PLUS_SIGN)
const minus = char(MINUS_SIGN)
const period = char(PERIOD)

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

export const ADD = '+'
export const SUBTRACT = '-'
export const MULTIPLY = '*'
export const DIVIDE = '/'
export const RAISE = '^'
export const RAISE_ALT = '**'
export const OPEN_PARENS = '('
export const CLOSE_PARENS = ')'
export const EQUALS = '='

const add = spaced(char(ADD))
const subtract = spaced(char(SUBTRACT))
const multiply = spaced(char(MULTIPLY))
const divide = spaced(char(DIVIDE))
const raise = spaced(or(char(RAISE), charSequence(RAISE_ALT)))
const openParens = spaced(char(OPEN_PARENS))
const closeParens = spaced(char(CLOSE_PARENS))
const equals = spaced(char(EQUALS))

export type OperatorType =
  | typeof ADD
  | typeof SUBTRACT
  | typeof MULTIPLY
  | typeof DIVIDE
  | typeof RAISE
  | typeof RAISE_ALT

export type RangeType = { type: 'range'; from: RefType; to: RefType }

type BinaryOperationType = {
  type: 'binaryOperation'
  left: ExpressionType
  operator: OperatorType
  right: ExpressionType
}
export type NumericType = { type: 'numeric'; value: number | string }
type ReferenceType = { type: 'reference'; ref: RefType }
type ParenthesizedExpressionType = { type: 'parenthesizedExpression'; expr: ExpressionType }
type FormulaFnCallType = {
  type: 'formulaFnCall'
  fnName: string
  parameters: (RangeType | ExpressionType)[]
}

export type ExpressionType =
  | BinaryOperationType
  | NumericType
  | ReferenceType
  | ParenthesizedExpressionType
  | FormulaFnCallType

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
    result.operator === SUBTRACT &&
    result.right.type === 'binaryOperation'
  ) {
    result.operator = ADD
    result.right.left = createBinaryOperation(NUMBER_MINUS_1, MULTIPLY, result.right.left)
  }

  return [result, rest]
}

const expression = additiveTerm

const multiplicativeTerm: Parser<ExpressionType> = input => {
  const [result, rest] = or(
    mapToBinaryOperation(
      and3(exponentialTerm, or(multiply, divide) as Parser<OperatorType>, multiplicativeTerm)
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
    result.operator === DIVIDE &&
    result.right.type === 'binaryOperation'
  ) {
    result.operator = MULTIPLY
    result.right.left = createBinaryOperation(NUMBER_1, DIVIDE, result.right.left)
  }

  return [result, rest]
}

const exponentialTerm: Parser<ExpressionType> = input =>
  or(
    mapToBinaryOperation(
      and3(factor as Parser<ExpressionType>, raise as Parser<OperatorType>, exponentialTerm)
    ),
    factor
  )(input) as ParserResult<ExpressionType>

const optionallySigned = <A extends ExpressionType>(parser: Parser<A>) =>
  map(and(optional(sign), parser), ([signChar, result]) =>
    signChar === MINUS_SIGN ? createBinaryOperation(NUMBER_MINUS_1, MULTIPLY, result) : result
  )

const operand = or(
  map(or3(binaryNumber, hexNumber, numeric), value => ({ type: 'numeric', value })),
  optionallySigned(map(ref, ref => ({ type: 'reference', ref })))
)

const parenthesizedExpression = optionallySigned(
  map(delimitedBy(openParens, expression, closeParens), expr => ({
    type: 'parenthesizedExpression',
    expr,
  }))
)

const colon = spaced(char(':'))
const comma = spaced(char(','))

const range: Parser<RangeType> = map(joinedBy(ref, colon), ([from, to]) => ({
  type: 'range',
  from,
  to,
}))

const fnParameter = or(range, expression)

const formulaFnCall = map(
  and(
    identifier,
    delimitedBy(
      openParens,
      optional(and(fnParameter, many(precededBy(comma, fnParameter)))),
      closeParens
    )
  ),
  ([fnName, params]) =>
    ({
      type: 'formulaFnCall',
      fnName,
      parameters: params === EMPTY_STRING ? [] : params.flat(),
    } as FormulaFnCallType)
)

const factor = or3(operand, parenthesizedExpression, formulaFnCall)

export const formula = precededBy(equals, expression)
