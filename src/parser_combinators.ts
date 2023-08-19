export const error = (msg: string) => new Error(msg)

export type ParserResult<T> = [resultOrError: T | Error, rest: string]
export type Parser<T> = (input: string) => ParserResult<T>
export type SingleChar = string

export const isError = <T>(result: T | Error): result is Error => result instanceof Error

export const satisfy =
  (matchFn: (char: SingleChar) => boolean): Parser<string> =>
  input =>
    input.length > 0 && matchFn(input[0]) ? [input[0], input.slice(1)] : [error('no match'), input]

export const sequence =
  <A, B>(parser: Parser<A>, fn: (value: A) => Parser<B>): Parser<B> =>
  input => {
    const [result, rest] = parser(input)

    return isError(result) ? [result, input] : fn(result)(rest)
  }

export const or =
  <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<A | B> =>
  input => {
    const [result1, rest1] = parser1(input)

    return !isError(result1) ? [result1, rest1] : parser2(input)
  }
export const or2 = or

export const or3 = <A, B, C>(parser1: Parser<A>, parser2: Parser<B>, parser3: Parser<C>): Parser<A | B | C> =>
  or(parser1, or2(parser2, parser3))

export const or4 = <A, B, C, D>(
  parser1: Parser<A>,
  parser2: Parser<B>,
  parser3: Parser<C>,
  parser4: Parser<D>
): Parser<A | B | C | D> => or(parser1, or3(parser2, parser3, parser4))

export const or5 = <A, B, C, D, E>(
  parser1: Parser<A>,
  parser2: Parser<B>,
  parser3: Parser<C>,
  parser4: Parser<D>,
  parser5: Parser<E>
): Parser<A | B | C | D | E> => or(parser1, or4(parser2, parser3, parser4, parser5))

export const and = <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<[A, B]> =>
  sequence(parser1, result1 => map(parser2, result2 => [result1, result2]))
export const and2 = and

export const and3 = <A, B, C>(parser1: Parser<A>, parser2: Parser<B>, parser3: Parser<C>): Parser<[A, B, C]> =>
  map(and(parser1, and2(parser2, parser3)), ([result1, results]) => [result1, ...results])

export const and4 = <A, B, C, D>(
  parser1: Parser<A>,
  parser2: Parser<B>,
  parser3: Parser<C>,
  parser4: Parser<D>
): Parser<[A, B, C, D]> =>
  map(and(parser1, and3(parser2, parser3, parser4)), ([result1, results]) => [result1, ...results])

export const and5 = <A, B, C, D, E>(
  parser1: Parser<A>,
  parser2: Parser<B>,
  parser3: Parser<C>,
  parser4: Parser<D>,
  parser5: Parser<E>
): Parser<[A, B, C, D, E]> =>
  map(and(parser1, and4(parser2, parser3, parser4, parser5)), ([result1, results]) => [result1, ...results])

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

export const concat = (parser: Parser<string[]>): Parser<string> => map(parser, chars => chars.join(''))

export const precededBy = <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<B> =>
  map(and(parser1, parser2), ([_, result2]) => result2)

export const succeededBy = <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<A> =>
  map(and(parser1, parser2), ([result1, _]) => result1)

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

export const natural = map(precededBy(optional(plus), digits), digs =>
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
  and(integer, precededBy(period, natural)),
  ([int, nat]) => int + (nat === 0 ? 0 : (Math.sign(int) * nat) / 10 ** Math.trunc(Math.log10(nat) + 1))
)

export const numeric = or(float, integer)

export const operand = or(ref, numeric)

export const exp = and(operand, many(and(operator, operand)))

export const equals = character('=')

export const formula = precededBy(equals, exp)
