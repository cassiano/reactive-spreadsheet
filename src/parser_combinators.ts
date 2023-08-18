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

    return [error('(or) neither p1 nor p2 satisfied'), input]
  }
export const or2 = or
export const or3 =
  <A, B, C>(parser1: Parser<A>, parser2: Parser<B>, parser3: Parser<C>): Parser<A | B | C> =>
  input => {
    const [result1, rest1] = parser1(input)
    if (!isError(result1)) return [result1, rest1]

    const [result2, rest2] = parser2(input)
    if (!isError(result2)) return [result2, rest2]

    const [result3, rest3] = parser3(input)
    if (!isError(result3)) return [result3, rest3]

    return [error('(or) none of p1...p3 satisfied'), input]
  }
export const or4 =
  <A, B, C, D>(parser1: Parser<A>, parser2: Parser<B>, parser3: Parser<C>, parser4: Parser<D>): Parser<A | B | C | D> =>
  input => {
    const [result1, rest1] = parser1(input)
    if (!isError(result1)) return [result1, rest1]

    const [result2, rest2] = parser2(input)
    if (!isError(result2)) return [result2, rest2]

    const [result3, rest3] = parser3(input)
    if (!isError(result3)) return [result3, rest3]

    const [result4, rest4] = parser4(input)
    if (!isError(result4)) return [result4, rest4]

    return [error('(or) none of p1...p4 satisfied'), input]
  }
export const or5 =
  <A, B, C, D, E>(
    parser1: Parser<A>,
    parser2: Parser<B>,
    parser3: Parser<C>,
    parser4: Parser<D>,
    parser5: Parser<E>
  ): Parser<A | B | C | D | E> =>
  input => {
    const [result1, rest1] = parser1(input)
    if (!isError(result1)) return [result1, rest1]

    const [result2, rest2] = parser2(input)
    if (!isError(result2)) return [result2, rest2]

    const [result3, rest3] = parser3(input)
    if (!isError(result3)) return [result3, rest3]

    const [result4, rest4] = parser4(input)
    if (!isError(result4)) return [result4, rest4]

    const [result5, rest5] = parser5(input)
    if (!isError(result5)) return [result5, rest5]

    return [error('(or) none of p1...p5 satisfied'), input]
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
export const and2 = and
export const and3 =
  <A, B, C>(parser1: Parser<A>, parser2: Parser<B>, parser3: Parser<C>): Parser<[A, B, C]> =>
  input => {
    const [result1, rest1] = parser1(input)
    if (isError(result1)) return [result1, input]

    const [result2, rest2] = parser2(rest1)
    if (isError(result2)) return [result2, input]

    const [result3, rest3] = parser3(rest2)
    if (isError(result3)) return [result3, input]

    return [[result1, result2, result3], rest3]
  }
export const and4 =
  <A, B, C, D>(parser1: Parser<A>, parser2: Parser<B>, parser3: Parser<C>, parser4: Parser<D>): Parser<[A, B, C, D]> =>
  input => {
    const [result1, rest1] = parser1(input)
    if (isError(result1)) return [result1, input]

    const [result2, rest2] = parser2(rest1)
    if (isError(result2)) return [result2, input]

    const [result3, rest3] = parser3(rest2)
    if (isError(result3)) return [result3, input]

    const [result4, rest4] = parser4(rest3)
    if (isError(result4)) return [result4, input]

    return [[result1, result2, result3, result4], rest4]
  }
export const and5 =
  <A, B, C, D, E>(
    parser1: Parser<A>,
    parser2: Parser<B>,
    parser3: Parser<C>,
    parser4: Parser<D>,
    parser5: Parser<E>
  ): Parser<[A, B, C, D, E]> =>
  input => {
    const [result1, rest1] = parser1(input)
    if (isError(result1)) return [result1, input]

    const [result2, rest2] = parser2(rest1)
    if (isError(result2)) return [result2, input]

    const [result3, rest3] = parser3(rest2)
    if (isError(result3)) return [result3, input]

    const [result4, rest4] = parser4(rest3)
    if (isError(result4)) return [result4, input]

    const [result5, rest5] = parser5(rest4)
    if (isError(result5)) return [result5, input]

    return [[result1, result2, result3, result4, result5], rest5]
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
  sequence(parser1, result1 => map(parser2, result2 => [result1, result2]))

export const concat = (parser: Parser<string[]>): Parser<string> => map(parser, chars => chars.join(''))

export const letter = satisfy(char => /[a-z]/i.test(char))
export const digit = map(
  satisfy(char => /\d/.test(char)),
  digit => +digit
)
export const character = (singleChar: SingleChar) => satisfy(char => char === singleChar)

export const letters = many1(letter)
export const digits = many1(digit)

export const identifierChar = map(or3(letter, digit, character('_')), res => res.toString())
export const identifier = concat(many1(identifierChar))

export const OPERATIONS: { [name: string]: SingleChar } = {
  addition: '+',
  subtraction: '-',
  multiplication: '*',
  division: '/',
  exponentiation: '^',
}

export const empty: Parser<string> = input => ['', input]
export const optional = <T>(parser: Parser<T>) => or(parser, empty)
export const ref = map(and(letters, digits), result => result.flat(2).join(''))
export const operator = or5(
  character(OPERATIONS.addition),
  character(OPERATIONS.subtraction),
  character(OPERATIONS.multiplication),
  character(OPERATIONS.division),
  character(OPERATIONS.exponentiation)
)
export const plus = character('+')
export const minus = character('-')
export const sign = or(plus, minus)
export const natural = map(and(optional(plus), digits), ([_, digs]) =>
  digs.reduce((acc, dig, i) => acc + dig * 10 ** (digs.length - (i + 1)), 0)
)
export const integer = map(and(optional(sign), natural), ([signChar, nat]) => (signChar === '-' ? -1 : +1) * nat)
export const naturalGreaterThanZero: Parser<number> = input => {
  const [result, rest] = natural(input)

  if (isError(result)) return [result, input]
  if (!(result > 0)) return [error(`Number must be > 0, but was ${result}`), input]

  return [result, rest]
}
export const period = character('.')
export const float = map(
  and3(integer, period, natural),
  ([int, _, nat]) => int + (Math.sign(int) * nat) / 10 ** Math.trunc(Math.log10(nat) + 1)
)
export const numeric = or(float, integer)
export const operand = or(ref, numeric)
export const exp = and(operand, many(and(operator, operand)))
export const preceededBy = <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<B> =>
  map(and(parser1, parser2), ([_, result2]) => result2)
export const succeededBy = <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<A> =>
  map(and(parser1, parser2), ([result1, _]) => result1)
export const equals = character('=')
export const formula = preceededBy(equals, exp)
