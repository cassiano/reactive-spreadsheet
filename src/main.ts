import { CellsType, EffectsType, makeCellReactive, repeat } from './make_cell_reactive'
import {
  DimensionsType,
  SheetDataType,
  SheetType,
  asRef,
  colAsLabel,
  loadSheet,
  sequenceReduce,
  sheetDimensions,
} from './spreadsheet_utils'
import './style.css'

// Spiral Fibonacci sequence.
const sheetData: SheetDataType = sequenceReduce(
  17,
  (acc, i) => ({
    ...acc,
    [`A${i + 2}`]: `=A${i + 1}+1`, // Col A: N
    [`B${i + 2}`]: `=B${i + 1}*A${i + 2}`, // Col B: N!
    [`C${i + 2}`]: `=1/B${i + 2}`, // Col C: 1 / N!
    [`D${i + 2}`]: `=SUM(C1:C${i + 2})`, // Col D: Σ (1 / N!), 0 <= N < ∞
  }),
  {
    A1: 0,
    B1: 1,
    C1: '=1/B1',
    D1: '=SUM(C1:C1)',
  }
)

const sheet: SheetType = loadSheet(sheetData, true)
const dimensions: DimensionsType = sheetDimensions(sheet)

const template = `
  <table>
    <tr>
      <th></th>
      ${repeat(dimensions.cols, col => `<th>${colAsLabel(col + 1)}</th>`)}
      <th></th>
    </tr>
    ${repeat(
      dimensions.rows,
      row =>
        '<tr>' +
        `<td><bold>${row + 1}</bold></td>` +
        repeat(dimensions.cols, col => '<td>' + `<input size="20" id='${asRef([row + 1, col + 1])}'/>` + '</td>') +
        `<td><bold>${row + 1}</bold></td>` +
        '</tr>'
    )}
    <tr>
      <th></th>
      ${repeat(dimensions.cols, col => `<th>${colAsLabel(col + 1)}</th>`)}
      <th></th>
    </tr>
  </table>
`

document.querySelector<HTMLDivElement>('#app')!.innerHTML = template

const effects: EffectsType = {}
const cells: CellsType = Object.keys(sheet).reduce(
  (acc, ref) => ({ ...acc, [ref]: document.querySelector<HTMLInputElement>(`#${ref}`) }),
  {}
)

Object.entries(cells).forEach(([ref, el]) => makeCellReactive(ref, el, sheet, cells, effects, true))

cells['A1'].click()
