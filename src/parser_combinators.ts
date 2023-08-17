export const error = (msg: string) => new Error(msg)

export type ParserResult<T> = [resultOrError: T | Error, rest: string]
export type Parser<T> = (input: string) => ParserResult<T>
export type SingleChar = string

export const isError = <T>(result: T | Error): result is Error => result instanceof Error

export const satisfy =
  (matchFn: (char: SingleChar) => boolean): Parser<string> =>
  input =>
    input.length > 0 && matchFn(input[0]) ? [input[0], input.slice(1)] : [error('no match'), input]

export const or =
  <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<A | B> =>
  input => {
    const [result1, rest1] = parser1(input)
    if (!isError(result1)) return [result1, rest1]

    const [result2, rest2] = parser2(input)
    if (!isError(result2)) return [result2, rest2]

    return [error('not p1 nor p2'), input]
  }

export const and =
  <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<[A, B]> =>
  input => {
    const [result1, rest1] = parser1(input)
    if (isError(result1)) return [result1, input]

    const [result2, rest2] = parser2(rest1)
    if (isError(result2)) return [result2, input]

    return [[result1, result2], rest2]
  }

export const map =
  <A, B>(parser: Parser<A>, fn: (value: A) => B): Parser<B> =>
  input => {
    const [result, rest] = parser(input)

    return isError(result) ? [result, input] : [fn(result), rest]
  }

export const manyN =
  <A>(parser: Parser<A>, minOccurences: number = 0): Parser<A[]> =>
  input => {
    const [result, rest] = parser(input)

    if (isError(result)) return minOccurences > 0 ? [result, input] : [[], input]

    return map(manyN(parser, minOccurences - 1), results => [result, ...results])(rest)
  }

export const many = manyN
export const many0 = many
export const many1 = <A>(parser: Parser<A>): Parser<A[]> => manyN(parser, 1)
export const many2 = <A>(parser: Parser<A>): Parser<A[]> => manyN(parser, 2)

export const sequence =
  <A, B>(parser: Parser<A>, fn: (value: A) => Parser<B>): Parser<B> =>
  input => {
    const [result, rest] = parser(input)

    return isError(result) ? [result, input] : fn(result)(rest)
  }

// Equivalent to and().
export const tuple = <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<[A, B]> =>
  sequence(parser1, r1 => map(parser2, r2 => [r1, r2]))

export const concat = (parser: Parser<string[]>): Parser<string> => map(parser, chars => chars.join(''))

export const letter = satisfy(char => /[a-z]/i.test(char))
export const digit = map(
  satisfy(char => /\d/.test(char)),
  digit => +digit
)
export const character = (singleChar: SingleChar) => satisfy(char => char === singleChar)

export const letters = many1(letter)
export const digits = many1(digit)

export const identifierChar = map(or(or(letter, digit), character('_')), res => res.toString())
export const identifier = concat(many1(identifierChar))

export const OPERATIONS: { [name: string]: SingleChar } = {
  addition: '+',
  subtraction: '-',
  multiplication: '*',
  division: '/',
  exponentiation: '^',
}

export const empty = (input: string): ParserResult<string> => ['', input]
export const optional = <T>(parser: Parser<T>) => or(parser, empty)
export const equals = character('=')
export const ref = map(and(letters, digits), result => result.flat(2).join(''))
export const operator = or(
  or(
    or(or(character(OPERATIONS.addition), character(OPERATIONS.subtraction)), character(OPERATIONS.multiplication)),
    character(OPERATIONS.division)
  ),
  character(OPERATIONS.exponentiation)
)
export const plusSign = character('+')
export const minusSign = character('+')
export const sign = or(plusSign, minusSign)
export const naturalNumber = map(and(optional(plusSign), digits), ([_, digs]) =>
  digs.reduce((acc, dig, i) => acc + dig * 10 ** (digs.length - 1 - i), 0)
)
export const integer = map(
  and(optional(sign), naturalNumber),
  ([signChar, natural]) => (signChar === '-' ? -1 : +1) * natural
)
export const naturalNumberGreaterThanZero: Parser<number> = (input: string) => {
  const [result, rest] = naturalNumber(input)

  if (isError(result)) return [result, input]
  if (!(result > 0)) return [error(`Number must be > 0, but was ${result}`), input]

  return [result, rest]
}
export const operand = or(ref, integer)
export const exp = and(operand, many(and(operator, operand)))
export const formula = and(equals, exp)
