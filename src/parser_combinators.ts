////////////////////////
// Parser combinators //
////////////////////////

import { RefType, SheetType, addCell } from './spreadsheet_utils'

const error = (msg: string) => new Error(msg)

type ParserResult<T> = [resultOrError: T | Error, rest: string]
type Parser<T> = (input: string) => ParserResult<T>
type SingleChar = string

const isError = <T>(result: T | Error): result is Error => result instanceof Error

const satisfy =
  (matchFn: (char: SingleChar) => boolean): Parser<string> =>
  input =>
    input.length > 0 && matchFn(input[0]) ? [input[0], input.slice(1)] : [error('no match'), input]

const or =
  <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<A | B> =>
  input => {
    const [result1, rest1] = parser1(input)
    if (!isError(result1)) return [result1, rest1]

    const [result2, rest2] = parser2(rest1)
    if (!isError(result2)) return [result2, rest2]

    return [error('not p1 nor p2'), input]
  }

const and =
  <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<[A, B]> =>
  input => {
    const [result1, rest1] = parser1(input)
    if (isError(result1)) return [result1, input]

    const [result2, rest2] = parser2(rest1)
    if (isError(result2)) return [result2, rest1]

    return [[result1, result2], rest2]
  }

const map =
  <A, B>(parser: Parser<A>, fn: (value: A) => B): Parser<B> =>
  input => {
    const [result, rest] = parser(input)

    return isError(result) ? [result, input] : [fn(result), rest]
  }

const manyN =
  <A>(parser: Parser<A>, n: number = 0): Parser<A[]> =>
  input => {
    const [result, rest] = parser(input)

    if (isError(result)) return n > 0 ? [result, input] : [[], input]

    return map(manyN(parser, n - 1), results => [result, ...results])(rest)
  }

const many = manyN
const many0 = many
const many1 = <A>(parser: Parser<A>): Parser<A[]> => manyN(parser, 1)
const many2 = <A>(parser: Parser<A>): Parser<A[]> => manyN(parser, 2)

const sequence =
  <A, B>(parser: Parser<A>, fn: (value: A) => Parser<B>): Parser<B> =>
  input => {
    const [result, rest] = parser(input)

    return isError(result) ? [result, input] : fn(result)(rest)
  }

// Equivalent to and().
const tuple = <A, B>(parser1: Parser<A>, parser2: Parser<B>): Parser<[A, B]> =>
  sequence(parser1, r1 => map(parser2, r2 => [r1, r2]))

const concat = (parser: Parser<string[]>): Parser<string> => map(parser, chars => chars.join(''))

const letter = satisfy(char => /[a-z]/i.test(char))
const digit = map(
  satisfy(char => /\d/.test(char)),
  digit => +digit
)
const character = (singleChar: SingleChar) => satisfy(char => char === singleChar)

const letters = many1(letter)
const digits = many1(digit)

const identifierChar = map(or(or(letter, digit), character('_')), res => res.toString())
const identifier = concat(many(identifierChar))

const inChunksOf = <T>(collection: T[], size: number): T[][] => {
  const results: T[][] = []

  for (let i = 0; i < collection.length; i += size) results.push(collection.slice(i, i + size))

  return results
}

const OPERATIONS = {
  addition: '+',
  subtraction: '-',
  multiplication: '*',
  division: '/',
  exponentiation: '^',
}

const empty = (input: string): ParserResult<string> => ['', input]
const optional = <T>(parser: Parser<T>) => or(parser, empty)
const equals = character('=')
const ref = map(and(letters, digits), r => r.flat(2).join(''))
const operator = or(
  or(
    or(or(character(OPERATIONS.addition), character(OPERATIONS.subtraction)), character(OPERATIONS.multiplication)),
    character(OPERATIONS.division)
  ),
  character(OPERATIONS.exponentiation)
)
const sign = or(character('+'), character('-'))
const integer = map(
  and(optional(sign), digits),
  ([signChar, digs]) =>
    (signChar === '-' ? -1 : +1) * digs.reduce((acc, dig, i) => acc + dig * 10 ** (digs.length - 1 - i), 0)
)
const operand = or(ref, integer)
const exp = and(operand, many(and(operator, operand)))
const formula = and(equals, exp)

const findOrCreateAndEvaluateCell = (sheet: SheetType, ref: RefType) => {
  if (!(ref in sheet.cells)) addCell(sheet, ref, () => 0)

  return sheet.cells[ref].signalWrapper()
}

export const evaluateFormula = (sheet: SheetType, value: string): number => {
  const match = formula(value.trim())
  if (isError(match) || match[1] !== '') throw new Error(`Invalid formula ${value}`)

  const match0 = match[0]
  if (isError(match0)) throw new Error(`Invalid formula ${value}`)

  const leftMostOperand = match0[1][0]
  const rest = match0[1][1].flat(2)

  const v1 =
    typeof leftMostOperand === 'number'
      ? leftMostOperand
      : findOrCreateAndEvaluateCell(sheet, leftMostOperand.toUpperCase())

  const restInChunksOf2 = inChunksOf(rest, 2) as [operator: SingleChar, operand: string | number][]

  return restInChunksOf2.reduce((acc, [operator, operand]) => {
    const v2 = typeof operand === 'number' ? operand : findOrCreateAndEvaluateCell(sheet, operand.toUpperCase())

    switch (operator) {
      case OPERATIONS.addition:
        return acc + v2
      case OPERATIONS.subtraction:
        return acc - v2
      case OPERATIONS.multiplication:
        return acc * v2
      case OPERATIONS.division:
        return acc / v2
      case OPERATIONS.exponentiation:
        return acc ** v2
      default:
        throw new Error(`Invalid operator ${operator}`)
    }
  }, v1)
}
