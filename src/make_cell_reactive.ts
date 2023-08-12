import { refreshSheet, sheetHasExpanded, setFocusedRef, CellInputsType, EffectsType } from './spreadsheet'
import { debug, effect } from './signals'
import {
  RefType,
  SheetType,
  asCoords,
  asRef,
  evaluateFormula,
  addCell,
  updateCellFormula,
  updateCellValue,
} from './spreadsheet_utils'

let shiftPressed: boolean = false

export const makeCellNavigable = (ref: RefType, el: HTMLInputElement, sheet: SheetType, cellnputs: CellInputsType) => {
  const { row, col } = asCoords(ref)

  ref = ref.toUpperCase()

  el.addEventListener('keydown', (e: KeyboardEvent) => {
    let targetRef: RefType | undefined

    switch (e.key) {
      case 'Enter':
      case 'ArrowDown':
        targetRef = asRef([(row % sheet.rows) + 1, row < sheet.rows ? col : (col % sheet.cols) + 1])

        setFocusedRef(targetRef)
        setTimeout(() => cellnputs[targetRef!].focus(), 0)
        break

      case 'ArrowUp':
        targetRef = asRef([
          ((sheet.rows + row - 2) % sheet.rows) + 1,
          row > 1 ? col : ((sheet.cols + col - 2) % sheet.cols) + 1,
        ])

        cellnputs[targetRef].focus()
        break

      case 'Tab': {
        if (shiftPressed && row === 1 && col === 1) {
          e.preventDefault()

          cellnputs[asRef([sheet.rows, sheet.cols])].focus()
        } else if (!shiftPressed && row === sheet.rows && col === sheet.cols) {
          e.preventDefault()

          cellnputs['A1'].focus()
        }

        break
      }

      case 'Shift':
        shiftPressed = true
        break

      case 'Escape':
        const sheetRef = sheet.cells[ref]

        cellnputs[ref].value =
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

  el.addEventListener('focus', () => {
    setFocusedRef(ref)
  })
}

let effectVersion: { [ref: RefType]: number } = {}

export const makeCellReactive = (
  ref: RefType,
  el: HTMLInputElement,
  sheet: SheetType,
  cellInputs: CellInputsType,
  effects: EffectsType
) => {
  ref = ref.toUpperCase()
  effectVersion[ref] ??= 1

  effects[ref] = effect(`${ref}-updater-v${effectVersion[ref]++}`, () => {
    const value = evaluateFormula(sheet, `=${ref}`)

    debug(() => `Updating cell ${ref} with value ${value}`)

    el.value = value.toString()

    markCellAsChanged(el)
  })

  el.addEventListener('change', (e: Event) => {
    const target = e.target as HTMLInputElement
    const value = target.value.trim()

    resetCellInputsColors(cellInputs)

    if (value.startsWith('=')) updateCellFormula(sheet, ref, value)
    else updateCellValue(sheet, ref, +value)

    if (sheetHasExpanded()) refreshSheet()
  })

  let focusTimeoutId: number | null

  const selectCell = (): number =>
    setTimeout(() => {
      focusTimeoutId = null

      cellInputs[ref].select()
    }, 0)

  el.addEventListener('focus', (e: Event) => {
    const target = e.target as HTMLInputElement

    setFocusedRef(ref)

    const sheetRef = sheet.cells[ref]

    if (sheetRef.formula !== undefined) target.value = sheetRef.formula.toString()

    focusTimeoutId = selectCell()
  })

  el.addEventListener('blur', (e: Event) => {
    const target = e.target as HTMLInputElement

    if (focusTimeoutId !== null) clearTimeout(focusTimeoutId)

    const sheetRef = sheet.cells[ref]

    if (sheetRef.formula !== undefined) target.value = sheetRef.signalWrapper().toString()
  })

  el.addEventListener('click', () => {
    selectCell()
  })
}

export const makeCellAutoReactive = (
  ref: RefType,
  el: HTMLInputElement,
  sheet: SheetType,
  cellInputs: CellInputsType,
  effects: EffectsType
) => {
  ref = ref.toUpperCase()

  const changeListener = (e: Event) => {
    const target = e.target as HTMLInputElement
    const value = target.value.trim()

    if (!(ref in sheet.cells)) addCell(sheet, ref, () => 0)

    resetCellInputsColors(cellInputs)

    if (value.startsWith('=')) updateCellFormula(sheet, ref, value)
    else updateCellValue(sheet, ref, +value)

    target.removeEventListener('change', changeListener)

    makeCellReactive(ref, target, sheet, cellInputs, effects)

    if (sheetHasExpanded()) refreshSheet()
  }

  el.addEventListener('change', changeListener)
}

const markCellAsChanged = (el: HTMLInputElement) => {
  el.style.backgroundColor = 'Yellow'
}

const markCellAsUnchanged = (el: HTMLInputElement) => {
  el.style.backgroundColor = 'White'
}

const resetCellInputsColors = (cellInputs: CellInputsType) => {
  Object.values(cellInputs).forEach(markCellAsUnchanged)
}
