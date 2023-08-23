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

const or3 = <A, B, C>(parserA: Parser<A>, parserB: Parser<B>, parserC: Parser<C>): Parser<A | B | C> =>
  or(parserA, or2(parserB, parserC))

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

const and3 = <A, B, C>(parserA: Parser<A>, parserB: Parser<B>, parserC: Parser<C>): Parser<[A, B, C]> =>
  map(and(parserA, and2(parserB, parserC)), ([resultA, otherResults]) => [resultA, ...otherResults])

const and4 = <A, B, C, D>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>,
  parserD: Parser<D>
): Parser<[A, B, C, D]> =>
  map(and(parserA, and3(parserB, parserC, parserD)), ([resultA, otherResults]) => [resultA, ...otherResults])

const and5 = <A, B, C, D, E>(
  parserA: Parser<A>,
  parserB: Parser<B>,
  parserC: Parser<C>,
  parserD: Parser<D>,
  parserE: Parser<E>
): Parser<[A, B, C, D, E]> =>
  map(and(parserA, and4(parserB, parserC, parserD, parserE)), ([resultA, otherResults]) => [resultA, ...otherResults])

type ManyOccurencesType = { minOccurences?: number; maxOccurences?: number }

const manyN =
  <A>(parser: Parser<A>, { minOccurences = 0, maxOccurences = Infinity }: ManyOccurencesType = {}): Parser<A[]> =>
  input => {
    if (maxOccurences === 0) return [[], input]

    const [result, rest] = parser(input)

    if (isError(result)) return minOccurences > 0 ? [result, input] : [[], input]

    return map(manyN(parser, { minOccurences: minOccurences - 1, maxOccurences: maxOccurences - 1 }), otherResults => [
      result,
      ...otherResults,
    ])(rest)
  }

const many = manyN
const many0 = many

const many1 = <A>(parser: Parser<A>, { maxOccurences = Infinity }: ManyOccurencesType = {}): Parser<A[]> =>
  manyN(parser, { minOccurences: 1, maxOccurences })

const many2 = <A>(parser: Parser<A>, { maxOccurences = Infinity }: ManyOccurencesType = {}): Parser<A[]> =>
  manyN(parser, { minOccurences: 2, maxOccurences })

const empty: Parser<EmptyString> = input => ['', input]

const optional = <A>(parser: Parser<A>): Parser<A | EmptyString> => or(parser, empty)
// const optional = <A>(parser: Parser<A>): Parser<A | EmptyString> =>
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

const charSequence =
  (w: string): Parser<string> =>
  input =>
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
    precededBy(doubleQuote, succeededBy(many(allButChar(DOUBLE_QUOTE)), doubleQuote)),
    precededBy(singleQuote, succeededBy(many(allButChar(SINGLE_QUOTE)), singleQuote)),
    precededBy(backTick, succeededBy(many(allButChar(BACK_TICK)), backTick))
  )
)

const wordChar = map(or3(letter, digit, underscore), res => res.toString())
const word = concat(many1(wordChar))

const ref = map(and(letters, digits), result => result.flat(2).join(''))

const sign = or(plus, minus)

const natural = map(precededBy(optional(plus), digits), digs =>
  digs.reduce((acc, dig, i) => acc + dig * 10 ** (digs.length - (i + 1)), 0)
)

const integer = map(and(optional(sign), natural), ([signChar, nat]) => (signChar === MINUS_SIGN ? -1 : 1) * nat)

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

export const ADD = '+'
export const SUBTRACT = '-'
export const MULTIPLY = '*'
export const DIVIDE = '/'
export const EXPONENTIATE = '^'
export const EXPONENTIATE_ALT = '**'
export const OPEN_PARENS = '('
export const CLOSE_PARENS = ')'

const add = char(ADD)
const subtract = char(SUBTRACT)
const multiply = char(MULTIPLY)
const divided = char(DIVIDE)
const exponentiate = or(char(EXPONENTIATE), charSequence(EXPONENTIATE_ALT))
const openParens = char(OPEN_PARENS)
const closeParens = char(CLOSE_PARENS)

export type OperatorType =
  | typeof ADD
  | typeof SUBTRACT
  | typeof MULTIPLY
  | typeof DIVIDE
  | typeof EXPONENTIATE
  | typeof EXPONENTIATE_ALT
export type ExpressionType =
  | (string | number)
  | [ExpressionType, OperatorType, ExpressionType]
  | [typeof OPEN_PARENS, ExpressionType, typeof CLOSE_PARENS]

// https://stackoverflow.com/questions/2969561/how-to-parse-mathematical-expressions-involving-parentheses
//
// const additionSubtractionTerm: Parser<ExpressionType> = input => or(and3(multiplicationDivisionTerm, or(plus, minus), expression), multiplicationDivisionTerm)(input)
// const multiplicationDivisionTerm: Parser<ExpressionType> = input => or(and3(exponentiationTerm, or(times, dividedBy), multiplicationDivisionTerm), exponentiationTerm)(input)
// const exponentiationTerm: Parser<ExpressionType> = input => or(and3(factor, toThePowerOf, exponentiationTerm), factor)(input)
// const factor = or(operand, succeededBy(precededBy(openParens, expression), closeParens))
// const expression = additionSubtractionTerm

const optionallySigned = <A>(parser: Parser<A>) =>
  map(and(optional(sign), parser), ([signChar, result]) =>
    signChar === MINUS_SIGN ? ([-1, MULTIPLY, result] as const) : result
  )

const operand = or(numeric, optionallySigned(ref))

const additiveTerm: Parser<ExpressionType> = input => {
  const [result, rest] = or(
    and3(multiplicativeTerm, or(add, subtract), additiveTerm),
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
  if (Array.isArray(result) && result[1] === SUBTRACT && Array.isArray(result[2])) {
    result[1] = ADD

    if (result[2][0] === OPEN_PARENS && result[2][2] === CLOSE_PARENS) {
      result[2] = [-1, MULTIPLY, result[2][1]]
    } else {
      result[2][0] = [-1, MULTIPLY, result[2][0]]
    }
  }

  return [result, rest]
}

const multiplicativeTerm: Parser<ExpressionType> = input => {
  const [result, rest] = or(
    and3(exponentialTerm, or(multiply, divided), multiplicativeTerm),
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
  if (Array.isArray(result) && result[1] === DIVIDE && Array.isArray(result[2])) {
    result[1] = MULTIPLY

    if (result[2][0] === OPEN_PARENS && result[2][2] === CLOSE_PARENS) {
      result[2] = [1, DIVIDE, result[2][1]]
    } else {
      result[2][0] = [1, DIVIDE, result[2][0]]
    }
  }

  return [result, rest]
}

const exponentialTerm: Parser<ExpressionType> = input => {
  const [result, rest] = or(and3(factor, exponentiate, exponentialTerm), factor)(input) as ParserResult<ExpressionType>

  if (isError(result)) return [result, input]

  return [result, rest]
}

const factor = or(operand, optionallySigned(and3(openParens, additiveTerm, closeParens)))

const expression = additiveTerm

export const formula = precededBy(equals, expression)
