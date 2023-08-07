import { IComputedSignalWrapper, effect, times } from './signals'
import {
  DimensionsType,
  RefType,
  SheetType,
  asCoords,
  asRef,
  evaluateFormula,
  sheetDimensions,
  updateCellFormula,
  updateCellValue,
} from './spreadsheet_utils'

export type CellsType = {
  [ref: RefType]: HTMLInputElement
}

export type EffectsType = {
  [ref: RefType]: IComputedSignalWrapper<void>
}

export const repeat = (count: number, fn: (i: number) => string) => times(count, fn).join('')

let shiftPressed: boolean = false

export const makeCellReactive = (
  ref: RefType,
  el: HTMLInputElement,
  sheet: SheetType,
  cells: CellsType,
  effects: EffectsType,
  displayZerosAsBlankCells = false
) => {
  const dimensions: DimensionsType = sheetDimensions(sheet)

  ref = ref.toUpperCase()

  effects[ref] = effect(`${ref} cell updater`, () => {
    const displayValue = evaluateFormula(sheet, `=${ref}`)

    el.value = displayZerosAsBlankCells && displayValue === 0 ? '' : displayValue.toString()
    el.style.backgroundColor = 'Gold'
  })

  el.addEventListener('change', (e: Event) => {
    const target = e.target as HTMLInputElement
    const value = target.value.trim()

    Object.values(cells).forEach(el => (el.style.backgroundColor = 'White'))

    if (value.startsWith('=')) updateCellFormula(sheet, ref, value)
    else updateCellValue(sheet, ref, +value)
  })

  el.addEventListener('focus', (e: Event) => {
    const target = e.target as HTMLInputElement
    const sheetRef = sheet[ref]

    if (sheetRef.formula !== undefined) {
      target.value = sheetRef.formula.toString()

      cells[ref].click()
    }
  })

  el.addEventListener('blur', (e: Event) => {
    const target = e.target as HTMLInputElement
    const sheetRef = sheet[ref]

    if (sheetRef.formula !== undefined) target.value = sheet[ref].signalWrapper().toString()
  })

  el.addEventListener('keyup', (e: KeyboardEvent) => {
    if (e.key === 'Shift') shiftPressed = false
  })

  el.addEventListener('keydown', (e: KeyboardEvent) => {
    let targetRef: RefType | undefined
    const { row, col } = asCoords(ref)

    switch (e.key) {
      case 'Enter':
      case 'ArrowDown':
        targetRef = asRef([(row % dimensions.rows) + 1, row < dimensions.rows ? col : (col % dimensions.cols) + 1])

        cells[targetRef].click()
        break

      case 'ArrowUp':
        targetRef = asRef([
          ((dimensions.rows + row - 2) % dimensions.rows) + 1,
          row > 1 ? col : ((dimensions.cols + col - 2) % dimensions.cols) + 1,
        ])

        cells[targetRef].click()
        break

      case 'Tab':
        if (shiftPressed && row === 1 && col === 1) {
          e.preventDefault()

          cells[asRef([dimensions.rows, dimensions.cols])].click()
        } else if (!shiftPressed && row === dimensions.rows && col === dimensions.cols) {
          e.preventDefault()

          cells['A1'].click()
        }
        break

      case 'Shift':
        shiftPressed = true
        break

      case 'Escape':
        const sheetRef = sheet[ref]

        cells[ref].value =
          sheetRef.formula !== undefined ? sheetRef.formula.toString() : sheetRef.signalWrapper().toString()
        break
      default:
    }
  })

  el.addEventListener('click', () => {
    setTimeout(() => cells[ref].select(), 0)
  })
}
