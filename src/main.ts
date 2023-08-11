import { makeCellAutoReactive, makeCellNavigable, makeCellReactive } from './make_cell_reactive'
import {
  SheetDataType,
  SheetType,
  asRef,
  colAsLabel,
  deleteKeys,
  expandRange,
  generateSpiralSequence,
  loadSheet,
  repeat,
} from './spreadsheet_utils'
import './style.css'
import { RefType } from './spreadsheet_utils'
import { IComputedSignal } from './signals'

// ---------------------------------------------------------------------------------------------

///////////
// Types //
///////////

export type CellInputsType = {
  [ref: RefType]: HTMLInputElement
}

export type EffectsType = {
  [ref: RefType]: IComputedSignal<void>
}

type HTML = string
type FC<T> = (props: T) => HTML

type ColumnLabelTHsProps = {
  cols: number
}

type CellProps = {
  size: number
  row: number
  col: number
}

type SpreadSheetProps = {
  sheet: SheetType
}

// ---------------------------------------------------------------------------------------------

////////////////
// Components //
////////////////

const ColumnLabelTHs: FC<ColumnLabelTHsProps> = ({ cols }) => {
  return repeat(
    cols,
    col => `
      <th>
        ${colAsLabel(col + 1)}
      </th>
    `
  )
}

const Cell: FC<CellProps> = ({ size, row, col }) => {
  return `
    <input size="${size}" id='${asRef([row + 1, col + 1])}'/>
  `
}

const SpreadSheet: FC<SpreadSheetProps> = ({ sheet: { rows, cols } }) => {
  const size = Math.max(2, window.innerWidth / cols / 15)

  const columnLabels = ColumnLabelTHs({ cols })

  return `
    <table>
      <tr>
        <th></th>
        ${columnLabels}
        <th></th>
      </tr>
      ${repeat(
        rows,
        row => `
        <tr>
          <td><bold>${row + 1}</bold></td>
          ${repeat(
            cols,
            col => `
              <td>
                ${Cell({ size, row, col })}
              </td>
            `
          )}
          <td><bold>${row + 1}</bold></td>
        </tr>
      `
      )}
      <tr>
        <th></th>
        ${columnLabels}
        <th></th>
      </tr>
    </table>
  `
}

// ---------------------------------------------------------------------------------------------

const displaySheet = (sheet: SheetType, cellInputs: CellInputsType, effects: EffectsType) => {
  renderSheet(sheet)
  clearPreviousSheetData(cellInputs, effects)
  processSheetCells(sheet, cellInputs, effects)

  if (focusedRef !== null) cellInputs[focusedRef].click()
}

// ---------------------------------------------------------------------------------------------

const renderSheet = (sheet: SheetType) =>
  (document.querySelector<HTMLDivElement>('#app')!.innerHTML = SpreadSheet({ sheet }))

// ---------------------------------------------------------------------------------------------

const clearPreviousSheetData = (cellInputs: CellInputsType, effects: EffectsType) => {
  // Remove all effect signals as observers of the sheet cells.
  Object.entries(effects).forEach(([ref, effect]) => {
    sheet.cells[ref].signalWrapper.signal.removeObserver(effect)
  })

  // Delete all effects.
  deleteKeys(effects)

  // Delete all cell inputs.
  deleteKeys(cellInputs)
}

// ---------------------------------------------------------------------------------------------

const processSheetCells = (sheet: SheetType, cellInputs: CellInputsType, effects: EffectsType) => {
  const refs = expandRange('A1', asRef([sheet.rows, sheet.cols])).flat(2)

  refs.forEach(ref => {
    cellInputs[ref] = document.querySelector<HTMLInputElement>(`#${ref}`)!
  })

  Object.entries(cellInputs).forEach(([ref, el]) => {
    if (ref in sheet.cells) makeCellReactive(ref, el, sheet, cellInputs, effects)
    else makeCellAutoReactive(ref, el, sheet, cellInputs, effects)

    makeCellNavigable(ref, el, sheet, cellInputs)
  })
}

// ---------------------------------------------------------------------------------------------

// Factorial.
// const FACTORIAL_TERMS = 20
// const sheetData: SheetDataType = sequenceReduce(
//   FACTORIAL_TERMS - 1,
//   (acc, i) => ({
//     ...acc,
//     [`A${i + 2}`]: `=A${i + 1}+1`,
//     [`B${i + 2}`]: `=A${i + 2}*B${i + 1}`,
//   }),
//   { A1: 0, B1: 1 }
// )

// Reversed factorial.
// const FACTORIAL_TERMS = 20
// const sheetData: SheetDataType = sequenceReduce(
//   FACTORIAL_TERMS - 1,
//   (acc, i) => ({
//     ...acc,
//     [`A${i + 2}`]: `=A${i + 3}+1`,
//     [`B${i + 2}`]: `=A${i + 2} <= 1 ? 1 : A${i + 2}*B${i + 3}`,
//   }),
//   { A1: '=A2+1', B1: '=A1*B2' }
// )

// Vertical Fibonnaci.
// const FIBONACCI_TERMS = 20
// const sheetData: SheetDataType = sequenceReduce(
//   FIBONACCI_TERMS,
//   (acc, i) => ({
//     ...acc,
//     [`A${i + 3}`]: `=A${i + 1}+A${i + 2}`,
//   }),
//   {
//     A1: 0,
//     A2: 1,
//   }
// )

// Horizontal Fibonnaci.
// const FIBONACCI_TERMS = 20
// const sheetData: SheetDataType = sequenceReduce(
//   FIBONACCI_TERMS,
//   (acc, i) => ({
//     ...acc,
//     [`${colAsLabel(i + 3)}1`]: `=${colAsLabel(i + 1)}1+${colAsLabel(i + 2)}1`,
//   }),
//   {
//     A1: 0,
//     B1: 1,
//   }
// )

// Spiral Fibonacci.
// const SPIRAL_1ST_SEGMENT_SIZE = 20
// const sheetData: SheetDataType = generateSpiralSequence(
//   SPIRAL_1ST_SEGMENT_SIZE,
//   'south',
//   'left',
//   [{ A1: 0 }, { A2: 1 }],
//   (_i, previousRefs, _nextRef) => `=${previousRefs[previousRefs.length - 2]}+${previousRefs[previousRefs.length - 1]}`
// )

// Reversed spiral sequence.
const REVERSED_SEQUENCE_TERMS = 20
const sheetData: SheetDataType = generateSpiralSequence(
  REVERSED_SEQUENCE_TERMS,
  'east',
  'right',
  [{ A1: '=B1+1' }],
  (_i, _previousRefs, nextRef) => `=${nextRef}+1`
)

// Cell squares.
// const sheetData: SheetDataType = generateCellSquares(20, 'A1', 1)

// Euler calculation.
// const EULER_TERMS = 18
// const sheetData: SheetDataType = sequenceReduce(
//   EULER_TERMS - 1,
//   (acc, i) => ({
//     ...acc,
//     [`A${i + 2}`]: `=A${i + 1}+1`, // Col A: N
//     [`B${i + 2}`]: `=B${i + 1}*A${i + 2}`, // Col B: N!
//     [`C${i + 2}`]: `=1/B${i + 2}`, // Col C: 1 / N!
//     [`D${i + 2}`]: `=SUM(C1:C${i + 2})`, // Col D: Σ (1 / N!), 0 <= N < ∞
//   }),
//   {
//     A1: 0,
//     B1: 1,
//     C1: '=1/B1',
//     D1: '=SUM(C1:C1)',
//   }
// )

// Reversed Euler calculation.
// const EULER_TERMS = 18
// const sheetData: SheetDataType = sequenceReduce(
//   EULER_TERMS - 1,
//   (acc, i) => ({
//     ...acc,
//     [`A${i + 2}`]: `=A${i + 3}+1`, // Col A: N
//     [`B${i + 2}`]: `=A${i + 2} <= 1 ? 1 : B${i + 3}*A${i + 2}`, // Col B: N!
//     [`C${i + 2}`]: `=1/B${i + 2}`, // Col C: 1 / N!
//     [`D${i + 2}`]: `=1+SUM(C${i + 2}:C${EULER_TERMS})`, // Col D: 1 + Σ (1 / N!), 1 <= N < ∞
//   }),
//   {
//     A1: '=A2+1',
//     B1: '=A1 <= 1 ? 1 : B2*A1',
//     C1: '=1/B1',
//     D1: `=1+SUM(C1:C${EULER_TERMS})`,
//   }
// )

// Powers of 2.
// const POWERS_OF_2_TERMS = 20
// const sheetData: SheetDataType = sequenceReduce(
//   POWERS_OF_2_TERMS - 1,
//   (acc, i) => ({
//     ...acc,
//     [`A${i + 2}`]: `=A${i + 1}+1`, // N
//     [`B${i + 2}`]: `=SUM(B1:B${i + 1})+1`, // 2ˆN
//   }),
//   { A1: 0, B1: 1 }
// )

const sheet: SheetType = loadSheet(sheetData)
const sheetCellInputs: CellInputsType = {}
const cellEffects: EffectsType = {}
let visibleRows = 0
let visibleCols = 0
let focusedRef: RefType | null = 'A1'

export const setFocusedRef = (ref: RefType | null) => {
  focusedRef = ref
}

export const sheetHasExpanded = () => sheet.rows > visibleRows || sheet.cols > visibleCols

export const refreshSheet = () => {
  displaySheet(sheet, sheetCellInputs, cellEffects)

  visibleRows = sheet.rows
  visibleCols = sheet.cols
}

if (sheetHasExpanded()) refreshSheet()

// window.sheetAsJson = sheetAsJson
// window.sheetAsTable = sheetAsTable
// window.sheet = sheet
