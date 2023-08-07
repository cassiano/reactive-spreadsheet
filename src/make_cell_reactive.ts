import { ComputedSignalKind, IComputedSignalWrapper, computed, debug, effect } from './signals'
import {
  DimensionsType,
  RefType,
  SheetType,
  asCoords,
  asRef,
  evaluateFormula,
  updateCellFormula,
  updateCellValue,
} from './spreadsheet_utils'

export type CellsType = {
  [ref: RefType]: HTMLInputElement
}

export type EffectsType = {
  [ref: RefType]: IComputedSignalWrapper<void>
}

let shiftPressed: boolean = false

export const makeCellNavigable = (
  ref: RefType,
  el: HTMLInputElement,
  sheet: SheetType,
  cells: CellsType,
  dimensions: DimensionsType
) => {
  const { row, col } = asCoords(ref)

  ref = ref.toUpperCase()

  el.addEventListener('keydown', (e: KeyboardEvent) => {
    let targetRef: RefType | undefined

    switch (e.key) {
      case 'Enter':
      case 'ArrowDown':
        targetRef = asRef([(row % dimensions.rows) + 1, row < dimensions.rows ? col : (col % dimensions.cols) + 1])

        cells[targetRef].focus()
        break

      case 'ArrowUp':
        targetRef = asRef([
          ((dimensions.rows + row - 2) % dimensions.rows) + 1,
          row > 1 ? col : ((dimensions.cols + col - 2) % dimensions.cols) + 1,
        ])

        cells[targetRef].focus()
        break

      case 'Tab': {
        if (shiftPressed && row === 1 && col === 1) {
          e.preventDefault()

          cells[asRef([dimensions.rows, dimensions.cols])].focus()
        } else if (!shiftPressed && row === dimensions.rows && col === dimensions.cols) {
          e.preventDefault()

          cells['A1'].focus()
        }

        break
      }

      case 'Shift':
        shiftPressed = true
        break

      case 'Escape':
        const sheetRef = sheet[ref]

        cells[ref].value =
          ref in sheet
            ? sheetRef.formula !== undefined
              ? sheetRef.formula.toString()
              : sheetRef.signalWrapper().toString()
            : ''
        break
      default:
    }
  })

  el.addEventListener('keyup', (e: KeyboardEvent) => {
    if (e.key === 'Shift') shiftPressed = false
  })
}

export const makeCellReactive = (
  ref: RefType,
  el: HTMLInputElement,
  sheet: SheetType,
  cells: CellsType,
  effects: EffectsType
) => {
  ref = ref.toUpperCase()

  effects[ref] = effect(`${ref}-cell-updater`, () => {
    const displayValue = evaluateFormula(sheet, `=${ref}`)

    debug(() => `Updating cell ${ref} with value ${displayValue}`)

    el.value = displayValue.toString()
    markCellAsChanged(el)
  })

  el.addEventListener('change', (e: Event) => {
    const target = e.target as HTMLInputElement
    const value = target.value.trim()

    resetCellsColors(cells)

    if (value.startsWith('=')) updateCellFormula(sheet, ref, value)
    else updateCellValue(sheet, ref, +value)
  })

  let focusTimeoutId: number

  const selectCell = (): number => setTimeout(() => cells[ref].select(), 0)

  el.addEventListener('focus', (e: Event) => {
    const target = e.target as HTMLInputElement
    const sheetRef = sheet[ref]

    if (sheetRef.formula !== undefined) target.value = sheetRef.formula.toString()

    focusTimeoutId = selectCell()
  })

  el.addEventListener('blur', (e: Event) => {
    if (focusTimeoutId !== undefined) clearTimeout(focusTimeoutId)

    const target = e.target as HTMLInputElement
    const sheetRef = sheet[ref]

    if (sheetRef.formula !== undefined) target.value = sheet[ref].signalWrapper().toString()
  })

  el.addEventListener('click', () => {
    selectCell()
  })
}

export const makeCellAutoGenerated = (
  ref: RefType,
  el: HTMLInputElement,
  sheet: SheetType,
  cells: CellsType,
  effects: EffectsType
) => {
  ref = ref.toUpperCase()

  const changeListener = (e: Event) => {
    const target = e.target as HTMLInputElement
    const value = target.value.trim()

    sheet[ref] = {
      signalWrapper: computed(ref, () => 0, { kind: ComputedSignalKind.Eager }),
    }

    resetCellsColors(cells)

    if (value.startsWith('=')) updateCellFormula(sheet, ref, value)
    else updateCellValue(sheet, ref, +value)

    el.removeEventListener('change', changeListener)

    makeCellReactive(ref, el, sheet, cells, effects)
  }

  el.addEventListener('change', changeListener)
}

const markCellAsChanged = (el: HTMLInputElement) => {
  el.style.backgroundColor = 'Yellow'
}

const markCellAsUnchanged = (el: HTMLInputElement) => {
  el.style.backgroundColor = 'White'
}

const resetCellsColors = (cells: CellsType) => {
  Object.values(cells).forEach(markCellAsUnchanged)
}
