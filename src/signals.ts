// https://tsplay.dev/wXyYkN

///////////////
// Constants //
///////////////

let DEBUG = false

////////////////////////
// Helper functions 1 //
////////////////////////

export const log = console.log

export const debug = (messageOrFalse: () => string | false): void => {
  if (DEBUG) {
    const message = messageOrFalse()

    if (message === false) return

    log(message)
  }
}

////////////////
// Exceptions //
////////////////

export class CircularDependencyError extends Error {
  name = 'CircularDependencyError'
}

/////////////////////////////////
// Types, interfaces and enums //
/////////////////////////////////

export enum ComputedSignalKind {
  Lazy = 'Lazy',
  Eager = 'Eager',
}

export interface ISubject {
  observers: Set<IObserver> // a.k.a "consumers", "readers", "subscribers", "listeners" etc
  /** Indicates whether the subject is still in use by an observer or has been removed during a recomputation. */
  inUse: boolean

  addObserver(observer: IObserver, addRelatedSubject?: boolean): void
  removeObserver(observer: IObserver, removeRelatedSubject?: boolean): void
  hasObserver(observer: IObserver): boolean
  notifyObservers(): void
  expireDescendantObservers(): void
  hasDescendantEagerObservers(breadcrumbs?: ObserverPatternType[]): boolean
}

export interface IObserver {
  subjects: Set<ISubject> // a.k.a "producers", "writers", "publishers", "dependencies" etc

  addSubject(subject: ISubject, addRelatedObserver?: boolean): void
  removeSubject(subject: ISubject, removeRelatedObserver?: boolean): void
  hasSubject(subject: ISubject): boolean
  expire(): void
  recalculate(): void // a.k.a. "update"
  recalculateStillExpiredObservers(breadcrumbs?: ComputedSignal<any>[]): void
}

export type ObserverPatternType = IObserver | ISubject

export interface IBaseSignal<T> extends ISubject {
  label: string
  value: T
  version: number

  get(previousValue?: T): T
  processInContext(observer?: IObserver): void
}

export interface IComputedSignal<T> extends IBaseSignal<T>, IObserver {
  valueFn: (previousValue: T) => T
  defaultValue?: T
  initialized: boolean
  expired: boolean
  /** Indicates whether the signal is being recomputed or not (used in the detection of cycles). */
  isRecomputing: boolean
  kind: ComputedSignalKind

  hasLazyBehavior(): boolean
  hasEagerBehavior(): boolean
  set(newValueFn: (previousValue?: T) => T): void
}

export interface IWritableSignal<T> extends IBaseSignal<T> {
  set(newValue: T): void
  update(newImmutableValueFn: (previousValue: T) => T): void
  mutate(newMutableValueFn: (previousValue: T) => void): void
}

export interface IComputedSignalWrapper<T> {
  (): T
  signal: IComputedSignal<T>
  set(newValueFn: (previousValue?: T) => T): void
}

export interface IWritableSignalWrapper<T> {
  (): T
  signal: IWritableSignal<T>
  set(newValue: T): void
  update(newImmutableValueFn: (previousValue: T) => T): void
  mutate(newMutableValueFn: (previousValue: T) => void): void
}

export type SignalWrapperType<T> = IComputedSignalWrapper<T> | IWritableSignalWrapper<T>

export interface IComputedSignalOptions<T> {
  kind?: ComputedSignalKind
  defaultValue?: T
}

/////////////
// Classes //
/////////////

class ExecutionContext<T> {
  private readonly stack: T[] = []

  run(value: T, fn: () => void): void {
    this.push(value)
    fn()
    this.pop()
  }

  push(value: T) {
    this.stack.push(value)
  }
  pop() {
    return this.stack.pop()
  }
  peek() {
    return this.stack.at(-1)
  }
  isEmpty() {
    return this.stack.length === 0
  }
  reset() {
    this.stack.length = 0
  }
}

export abstract class BaseSignal<T> implements IBaseSignal<T> {
  value!: T
  version: number = 0
  inUse = true
  readonly observers: Set<IObserver> = new Set()

  static readonly executionContext = new ExecutionContext<IObserver>()

  constructor(public readonly label: string) {}

  ////////////////////////////////
  // ISignalBase implementation //
  ////////////////////////////////

  get() {
    const observer = BaseSignal.executionContext.peek()

    this.processInContext(observer)

    debug(
      () =>
        `Reading value ${this.value} of signal ${this.label} from signal ${BaseSignal.label(
          observer
        )}`
    )

    if (observer !== undefined) {
      if (!observer.hasSubject(this)) observer.addSubject(this)

      this.inUse = true
    }

    return this.value
  }

  abstract processInContext(_observer?: IObserver): void

  /////////////////////////////
  // ISubject implementation //
  /////////////////////////////

  addObserver(observer: IObserver, addRelatedSubject: boolean = true): void {
    debug(() => `Adding observer ${BaseSignal.label(observer)} to ${this.label}`)

    this.observers.add(observer)

    if (addRelatedSubject) observer.addSubject(this, false)
  }

  removeObserver(observer: IObserver, removeRelatedSubject: boolean = true): void {
    debug(() => `Removing observer ${BaseSignal.label(observer)} from ${this.label}`)

    if (!this.observers.delete(observer))
      throw new Error(
        `Error when removing observer ${BaseSignal.label(observer)} from ${this.label}`
      )

    if (removeRelatedSubject) observer.removeSubject(this, false)
  }

  hasObserver(observer: IObserver): boolean {
    return this.observers.has(observer)
  }

  notifyObservers(): void {
    debug(() => `Notifying ${this.label}'s observers: ${BaseSignal.labels(this.observers)}`)

    this.observers.forEach(observer => observer.recalculate())
  }

  expireDescendantObservers(): void {
    debug(
      () =>
        this.observers.size > 0 &&
        `Expiring ${this.label}'s observers: ${BaseSignal.labels(this.observers)}`
    )

    this.observers.forEach(observer => observer.expire())
  }

  hasDescendantEagerObservers(breadcrumbs: ObserverPatternType[] = []): boolean {
    debug(
      () =>
        breadcrumbs.length > 0 &&
        `hasDescendantEagerObservers()'s breadcrumbs for ${this.label}: ${BaseSignal.labels(
          breadcrumbs
        )}`
    )

    // Signal already visited/checked?
    if (breadcrumbs.indexOf(this) !== -1) {
      debug(() => `Skipping already visited signal/observer ${this.label}`)

      return false
    }

    const observersArray = [...this.observers]

    // Check if there is any direct eager observer or, if none found, if there is any direct lazy observer
    // that, acting as a subject, has itself a direct or indirect eager observer.
    const found =
      observersArray.some(
        observer => (observer as ComputedSignal<unknown>).kind === ComputedSignalKind.Eager
      ) ||
      observersArray.some(observer =>
        (observer as unknown as ISubject).hasDescendantEagerObservers(breadcrumbs)
      )

    if (!found) breadcrumbs.push(this) // Notice our breadcrumbs set contains only negative/false matches.

    return found
  }

  ///////////////////
  // Other methods //
  ///////////////////

  static label(
    signal: ObserverPatternType | undefined,
    defaultLabel = '(non-reactive context) main'
  ) {
    return (signal as IBaseSignal<unknown>)?.label ?? defaultLabel
  }

  static labels(
    signals: Set<ObserverPatternType> | ObserverPatternType[],
    separator = ', '
  ): string {
    return (
      '[' +
      [...signals].map(signal => this.label(signal as IBaseSignal<unknown>)).join(separator) +
      ']'
    )
  }
}

export class ComputedSignal<T> extends BaseSignal<T> implements IComputedSignal<T> {
  expired: boolean = true
  isRecomputing: boolean = false
  initialized: boolean = false
  readonly kind: ComputedSignalKind
  readonly defaultValue?: T
  readonly subjects: Set<ISubject> = new Set()

  constructor(
    label: string,
    public valueFn: (previousValue?: T) => T,
    options: IComputedSignalOptions<T> = {}
  ) {
    super(label)

    this.kind = options.kind ?? ComputedSignalKind.Lazy
    this.defaultValue = options.defaultValue

    if (this.kind === ComputedSignalKind.Eager) this.get()
  }

  ////////////////////////////////
  // ISignalBase implementation //
  ////////////////////////////////

  processInContext(observer?: IObserver): void {
    let previousSubjectsInUseValues: Map<ISubject, boolean> = new Map()

    if (!this.expired) return

    if (this.isRecomputing) {
      BaseSignal.executionContext.reset()

      throw new CircularDependencyError(
        `Circular dependency detected when reading signal ${
          this.label
        } from signal ${BaseSignal.label(observer as ComputedSignal<unknown>)}`
      )
    }

    this.isRecomputing = true

    // Save the in-use values of all current (but soon to become "previous") subjects.
    this.subjects.forEach(subject => previousSubjectsInUseValues.set(subject, subject.inUse))

    // Mark all subjects as invalid before recomputing the signal.
    this.subjects.forEach(subject => {
      subject.inUse = false
    })

    // Recompute it.
    BaseSignal.executionContext.run(this, () => {
      debug(() => `(Re)computing ${this.label}`)

      this.value = this.valueFn(this.initialized ? this.value : this.defaultValue)

      this.version++
      this.initialized = true
    })

    // Remove all (still) invalid subjects.
    ;[...this.subjects]
      .filter(subject => !subject.inUse)
      .forEach(subject => this.removeSubject(subject))

    // Restore the in-use values of *all* previous subjects, including removed ones.
    previousSubjectsInUseValues.forEach((inUse, subject) => {
      subject.inUse = inUse
    })

    this.expired = false
    this.isRecomputing = false
  }

  //////////////////////////////
  // IObserver implementation //
  //////////////////////////////

  addSubject(subject: ISubject, addRelatedObserver: boolean = true): void {
    debug(() => `Adding subject ${BaseSignal.label(subject)} to ${this.label}`)

    this.subjects.add(subject)

    if (addRelatedObserver) subject.addObserver(this, false)
  }

  removeSubject(subject: ISubject, removeRelatedObserver: boolean = true): void {
    debug(() => `Removing subject ${BaseSignal.label(subject)} from ${this.label}`)

    if (!this.subjects.delete(subject))
      throw new Error(`Error when removing subject ${BaseSignal.label(subject)} from ${this.label}`)

    if (removeRelatedObserver) subject.removeObserver(this, false)
  }

  hasSubject(subject: ISubject): boolean {
    return this.subjects.has(subject)
  }

  expire(): void {
    if (this.expired) return

    this.expired = true
    this.expireDescendantObservers()
  }

  recalculate(): void {
    if (!this.expired) return

    // Nothing to do if observer has effective lazy behavior (i.e. is lazy and has no descendant eager observers).
    if (this.hasLazyBehavior()) {
      debug(
        () =>
          `Skipping recalculation of observer ${this.label}, with effective lazy behavior (i.e. is lazy and has no descendant eager observers)`
      )

      return
    }

    const previousValue: T = this.value

    this.get()

    // If value has changed, notify its own observers to recursively update (recompute/expire).
    if (this.value !== previousValue) this.notifyObservers()
  }

  recalculateStillExpiredObservers(breadcrumbs: ComputedSignal<any>[] = []): void {
    debug(
      () =>
        this.observers.size > 0 &&
        `Recalculating ${this.label}'s observers: ${BaseSignal.labels(this.observers)}`
    )

    // Signal already visited/checked?
    if (breadcrumbs.indexOf(this) !== -1) return

    breadcrumbs.push(this)

    this.observers.forEach(observer => {
      observer.recalculate()
      observer.recalculateStillExpiredObservers(breadcrumbs)
    })
  }

  ////////////////////////////////////
  // IComputedSignal implementation //
  ////////////////////////////////////

  hasLazyBehavior(): boolean {
    return this.kind === ComputedSignalKind.Lazy && !this.hasDescendantEagerObservers()
  }

  hasEagerBehavior(): boolean {
    // Equivalent to: `return this.type === ObserverTypeEnum.Eager || this.hasDescendantEagerObservers(`.
    return !this.hasLazyBehavior()
  }

  set(newValueFn: (previousValue?: T) => T) {
    this.valueFn = newValueFn
    this.expire()
    this.recalculate()
    this.recalculateStillExpiredObservers() // Usually unfired effects...
  }
}

export class WritableSignal<T> extends BaseSignal<T> {
  constructor(label: string, public value: T) {
    super(label)

    this.version++
  }

  ////////////////////////////////
  // ISignalBase implementation //
  ////////////////////////////////

  // There is nothing to process in context for a WritableSignal.
  processInContext(_observer?: IComputedSignal<unknown> | undefined): void {}

  ////////////////////////////////////
  // IWritableSignal implementation //
  ////////////////////////////////////

  set(newValue: T): void {
    this.change(newValue)
  }

  update(newImmutableValueFn: (previousValue: T) => T): void {
    this.change(newImmutableValueFn)
  }

  mutate(newMutableValueFn: (previousValue: T) => void): void {
    newMutableValueFn(this.value)

    this.change(this.value, true)
  }

  ///////////////////
  // Other methods //
  ///////////////////

  private change(newValueOrFn: T | ((previousValue: T) => T), skipEqualityCheck = false): void {
    const newValue = newValueOrFn instanceof Function ? newValueOrFn(this.value) : newValueOrFn

    if (skipEqualityCheck || newValue !== this.value) {
      if (skipEqualityCheck)
        debug(
          () =>
            `Replacing the (already mutated) value of ${this.label}: ${JSON.stringify(this.value)}`
        )
      else
        debug(
          () =>
            `Replacing the immutable value of ${this.label}: ${JSON.stringify(
              this.value
            )} -> ${JSON.stringify(newValue)}`
        )

      this.value = newValue
      this.version++

      this.expireDescendantObservers() // Notice this expires all direct and indirect observers at once.
      this.notifyObservers()
    }
  }
}

///////////////////////////////
// Factory/wrapper functions //
///////////////////////////////

export const signal = function <T>(label: string, initialValue: T): IWritableSignalWrapper<T> {
  const signal = new WritableSignal(label, initialValue)

  // Wrap the getter in the function call itself.
  const wrapper: IWritableSignalWrapper<T> = () => signal.get()

  // Simply delegate the set(), update() and mutate() methods to the signal.
  wrapper.set = (newValue: T) => signal.set(newValue)
  wrapper.update = (newImmutableValueFn: (previousValue: T) => T) =>
    signal.update(newImmutableValueFn)
  wrapper.mutate = (newMutableValueFn: (previousValue: T) => void) =>
    signal.mutate(newMutableValueFn)

  // Allow access to the underlying signal.
  wrapper.signal = signal

  return wrapper
}

export const computed = function <T>(
  label: string,
  valueFn: (previousValue?: T) => T,
  options: IComputedSignalOptions<T> = {}
): IComputedSignalWrapper<T> {
  const signal = new ComputedSignal(label, valueFn, options)

  // Wrap the getter in the function call itself.
  const wrapper: IComputedSignalWrapper<T> = () => signal.get()

  // Simply delegate the set() method to the signal.
  wrapper.set = (newValueFn: (previousValue?: T) => T) => signal.set(newValueFn)

  // Allow access to the underlying signal.
  wrapper.signal = signal

  return wrapper
}

export const effect = <T>(
  label: string,
  fn: (previousValue?: T) => T,
  defaultValue?: T
): IComputedSignal<T> =>
  computed(label, fn, { kind: ComputedSignalKind.Eager, defaultValue }).signal

// https://tsplay.dev/Nr37Vm
export const isComputedSignalWrapper = <T>(
  wrapper: SignalWrapperType<T>
): wrapper is IComputedSignalWrapper<T> => wrapper.signal instanceof ComputedSignal
export const isWritableSignalWrapper = <T>(
  wrapper: SignalWrapperType<T>
): wrapper is IWritableSignalWrapper<T> => wrapper.signal instanceof WritableSignal

////////////////////////
// Helper functions 2 //
////////////////////////

export function signalReplacerFn<T>(key: string, value: any) {
  if (key === 'observers' || key === 'subjects') {
    return [...value].map((s: IBaseSignal<T>) => s.label)
  } else if (value instanceof Function) {
    return '(function)'
  } else if (value === undefined) {
    return '(undefined)'
  } else {
    return value // return as is
  }
}

export const inspectSignal = <T>(signalOrWrapper: BaseSignal<T> | SignalWrapperType<T>) => {
  log(
    JSON.stringify(
      signalOrWrapper instanceof BaseSignal ? signalOrWrapper : signalOrWrapper.signal,
      signalReplacerFn<T>,
      2
    )
  )
}

export const subjects = (computedSignalWrapper: IComputedSignalWrapper<any>) =>
  [...computedSignalWrapper.signal.subjects].map(subject => BaseSignal.label(subject))

export const observers = (signalWrapper: SignalWrapperType<any>) =>
  [...signalWrapper.signal.observers].map(observer => BaseSignal.label(observer))

export const times = <T>(n: number, fn: (index: number) => T): T[] => [...Array(n).keys()].map(fn)

// const inspect = <T>({ signal }: SignalWrapperType<T>) => {
//   log(JSON.stringify(signal, signalReplacerFn<T>, 2))
// }

// const isEven = (n: number) => n % 2 === 0
// const isOdd = (n: number) => !isEven(n)

// const assertEquals = <T>(testId: number | string, expected: T, actual: T): void => {
//   const expectedJson = JSON.stringify(expected instanceof Array ? expected.sort() : expected)
//   const actualJson = JSON.stringify(actual instanceof Array ? actual.sort() : actual)

//   if (expectedJson !== actualJson)
//       throw new Error(`Test ${testId} failed: >>>>>>>>>> Expected ${expectedJson}, but got ${actualJson} instead <<<<<<<<<<`)
// }

// const assertRaisesException = <T extends Error>(
//   testId: number | string,
//   fn: () => void,
//   expectedExceptionClass: new () => T
// ) => {
//   let expectedExceptionRaised = false
//   let actualException: Error | undefined

//   try {
//       fn()
//   } catch (e: any) {
//       actualException = e

//       expectedExceptionRaised = actualException instanceof expectedExceptionClass
//   }

//   if (!expectedExceptionRaised) {
//       const errorMessage =
//           `Test ${testId} failed: >>>>>>>>>> Expected callback to raise exception "${expectedExceptionClass.name}", but ` +
//           (actualException !== undefined ?
//               `got exception "${actualException.name}" with message "${actualException.message}" instead <<<<<<<<<<` :
//               `none was raised <<<<<<<<<<`)

//       throw new Error(errorMessage)
//   }
// }

// let display = console.log

// const assertDisplays = (testId: number, fn: () => void, expectedMessage: string) => {
//   const previousDisplayFn = display
//   let actualMessage: string = ""

//   display = (msg: string) => actualMessage = msg

//   fn()

//   if (actualMessage !== expectedMessage)
//       throw new Error(`Test ${testId} failed: >>>>>>>>>> Expected '${expectedMessage}', but got '${actualMessage}' instead <<<<<<<<<<`)

//   display = previousDisplayFn
// }

// const subjects = (computedSignalWrapper: IComputedSignalWrapper<any>) =>
//   [...computedSignalWrapper.signal.subjects].map(subject => BaseSignal.label(subject))

// const observers = (signalWrapper: SignalWrapperType<any>) =>
//   [...signalWrapper.signal.observers].map(observer => BaseSignal.label(observer))

// const times = <T>(n: number, fn: (index: number) => T): T[] => [...Array(n).keys()].map(i => fn(i))

// /////////////////
// // Client code //
// /////////////////

// type TaskType = {
//   id: number;
//   title: string;
//   done: boolean;
// };

// type FilterType = 'all' | 'active' | 'completed';

// const tasks = signal<TaskType[]>("tasks", [
//   { id: 1, title: 'A', done: true },
//   { id: 2, title: 'B', done: false },
//   { id: 3, title: 'C', done: true },
// ])
// const filter = signal<FilterType>("filter", "active")

// const activeTasks = computed(
//   "activeTasks",
//   () => tasks().filter(task => !task.done)
// );

// const completedTasks = computed(
//   "completedTasks",
//   () => tasks().filter(task => task.done)
// );

// const visibleTasks = computed(
//   "visibleTasks",
//   () => {
//       const filterValue = filter();

//       switch (filterValue) {
//           case 'all':
//               return tasks();
//           case 'active':
//               return activeTasks();
//           case 'completed':
//               return completedTasks();
//           default:
//               const _exhaustiveCheck: never = filterValue;
//               return [];
//       }
//   }
// );

// const allSignals = computed(
//   "allSignals",
//   () => ({
//       filter: filter(),
//       tasks: tasks(),
//       visibleTasks: visibleTasks(),
//       activeTasks: activeTasks(),
//       completedTasks: completedTasks()
//   })
// );

// assertEquals(1, [{ id: 2, title: 'B', done: false }], activeTasks())
// assertEquals(2, [{ id: 1, title: 'A', done: true }, { id: 3, title: 'C', done: true },], completedTasks())
// assertEquals(3, [{ id: 2, title: 'B', done: false }], visibleTasks())
// assertEquals(4, ["tasks"], subjects(activeTasks))
// assertEquals(5, ["visibleTasks"], observers(activeTasks))
// assertEquals(6, ["tasks"], subjects(completedTasks))
// assertEquals(7, [], observers(completedTasks))
// assertEquals(8, ["filter", "activeTasks"], subjects(visibleTasks))
// assertEquals(9, [], observers(visibleTasks))

// allSignals()
// assertEquals(9.1, ["visibleTasks", "allSignals"], observers(activeTasks))
// assertEquals(9.2, ["allSignals"], observers(completedTasks))
// assertEquals(9.3, ["allSignals"], observers(visibleTasks))
// assertEquals(10, ["filter", "tasks", "visibleTasks", "activeTasks", "completedTasks"], subjects(allSignals))
// assertEquals(11, [], observers(allSignals))

// filter.set('completed')
// assertEquals(12, [{ id: 2, title: 'B', done: false }], activeTasks())
// assertEquals(13, [{ id: 1, title: 'A', done: true }, { id: 3, title: 'C', done: true },], completedTasks())
// assertEquals(14, [{ id: 1, title: 'A', done: true }, { id: 3, title: 'C', done: true },], visibleTasks())
// assertEquals(15, ["tasks"], subjects(activeTasks))
// assertEquals(16, ["allSignals"], observers(activeTasks))
// assertEquals(17, ["tasks"], subjects(completedTasks))
// assertEquals(18, ["visibleTasks", "allSignals"], observers(completedTasks))
// assertEquals(19, ["filter", "completedTasks"], subjects(visibleTasks))
// assertEquals(20, ["allSignals"], observers(visibleTasks))
// assertEquals(21, ["filter", "tasks", "visibleTasks", "activeTasks", "completedTasks"], subjects(allSignals))
// assertEquals(22, [], observers(allSignals))

// tasks.update(previousTasks => previousTasks.filter(task => task.id !== 1))     // Remove task with id = 1.
// assertEquals(23, [{ id: 2, title: 'B', done: false }], activeTasks())
// assertEquals(24, [{ id: 3, title: 'C', done: true },], completedTasks())
// assertEquals(25, [{ id: 3, title: 'C', done: true },], visibleTasks())

// tasks.mutate(previousTasks => previousTasks.push({ id: 4, title: 'D', done: true }))     // Add new task with id = 4.
// assertEquals(26, [{ id: 2, title: 'B', done: false }], activeTasks())
// assertEquals(27, [{ id: 3, title: 'C', done: true }, { id: 4, title: 'D', done: true }], completedTasks())
// assertEquals(28, [{ id: 3, title: 'C', done: true }, { id: 4, title: 'D', done: true }], visibleTasks())

// tasks.mutate(previousTasks => previousTasks[0].done = true)     // Mutate 1st task (B) to done = true.
// assertEquals(29, [], activeTasks())
// assertEquals(30, [{ id: 2, title: 'B', done: true }, { id: 3, title: 'C', done: true }, { id: 4, title: 'D', done: true }], completedTasks())
// assertEquals(31, [{ id: 2, title: 'B', done: true }, { id: 3, title: 'C', done: true }, { id: 4, title: 'D', done: true }], visibleTasks())

// // ----------------------------------------------------------------------------------------------------------

// /////////////////////////////
// // Fibonacci sequence test //
// /////////////////////////////

// const FIBONACCI_TERMS = 13

// const fibonacci: [
//   IWritableSignalWrapper<number>,
//   IWritableSignalWrapper<number>,
//   ...IComputedSignalWrapper<number>[]
// ] = [
//       signal("fib[0]", 0),
//       signal("fib[1]", 1)
//   ]

// times(FIBONACCI_TERMS - 2, i => {
//   const j = i + 2

//   fibonacci[j] = computed(
//       `fib[${j}]`,
//       () => fibonacci[j - 1]() + fibonacci[j - 2](),
//       { kind: ComputedSignalKind.Eager }
//   )
// })

// // Read in ascending order:
// // fibonacci.forEach(item => log(item.signal.label, item()))

// // Or in descending order:
// fibonacci.slice().reverse().forEach(item => log(item.signal.label, item()))

// log("Before recalculation of the last element:")
// inspect(fibonacci[FIBONACCI_TERMS - 1])

// assertEquals(32, 144, fibonacci[12]())

// // ----------------------------------------------------------------------------------------------------------

// const a1 = signal("a1", 1)
// const b1 = computed("b1", () => a1() * 2)
// const c1 = computed("c1", () => a1() + b1() * 3)

// // Create 'd1' such as 'b1' is accessed both conditionally+directly and indirectly via 'c1'.
// const d1 = computed("d1", () => c1() + (a1() === 1 ? b1() : 10))

// d1()
// assertEquals(33, ["c1", "a1", "b1"], subjects(d1))
// a1.set(2)
// d1()

// // 'b1' should not appear as a subject of 'd1' anymore.
// assertEquals(34, ["c1", "a1"], subjects(d1))

// a1.set(1)
// d1()

// // 'b1' should reappear as a subject of 'd1' once again.
// assertEquals(35, ["c1", "a1", "b1"], subjects(d1))

// // ----------------------------------------------------------------------------------------------------------

// const a2 = signal("a2", 1)
// const b2 = signal("b2", 2)
// const c2 = signal("c2", 3)
// let f2: IComputedSignalWrapper<number>
// const d2 = computed("d2", () => a2() + b2() + c2() + f2())
// const e2 = computed("e2", () => d2() * (isOdd(a2()) ? b2() * 2 : c2() * 3))
// f2 = computed("f2", () => e2() + 1)

// assertRaisesException(36, () => f2(), CircularDependencyError)

// // ----------------------------------------------------------------------------------------------------------

// //////////////////////////////////////////////////////////////
// // Euler number calulation test: e = Σ (1 / N!), 0 <= N < ∞ //
// //////////////////////////////////////////////////////////////

// const EULER_TERMS = 18

// // const factorial: ComputedSignalWrapperType<number>[] =
// //     times(EULER_TERMS, i =>
// //         computed(`factorial[${i}]`, () =>
// //             i <= 1 ? 1 : i * factorial[i - 1]()
// //         )
// //     )

// // const euler: ComputedSignalWrapperType<number>[] =
// //     times(EULER_TERMS, i =>
// //         computed(`euler[${i}]`, () =>
// //             (i === 0 ? 0 : euler[i - 1]()) + (1 / factorial[i]())
// //         )
// //     )

// // assertEquals(37, 2.7182818284590455, euler[EULER_TERMS - 1]())

// // factorial.slice(-3).forEach(inspect)
// // euler.slice(-3).forEach(inspect)

// // Euler as a 2D matrix:
// const euler: IComputedSignalWrapper<number>[][] = [[], []]

// times(EULER_TERMS, i => {
//   euler[0][i] = computed(
//       `euler[0][${i}]`,
//       () => i <= 1 ? 1 : i * euler[0][i - 1]()
//   )

//   euler[1][i] = computed(
//       `euler[1][${i}]`,
//       () => (i === 0 ? 0 : euler[1][i - 1]()) + (1 / euler[0][i]()),
//       { kind: ComputedSignalKind.Eager }
//   )
// })

// assertEquals(37, 2.7182818284590455, euler[1][EULER_TERMS - 1]())

// euler[0].slice(-3).forEach(inspect)
// euler[1].slice(-3).forEach(inspect)

// // ----------------------------------------------------------------------------------------------------------

// const a3 = signal("a3", 1)
// const b3 = signal("b3", 2)
// const c3 = signal("c3", 3)
// const d3 = computed("d3", () => a3() === 1 ? b3() * 2 : c3() * 3, { kind: ComputedSignalKind.Eager })

// const d3InitialVersion: number = d3.signal.version

// assertEquals(38, 4, d3())
// assertEquals(40, ["a3", "b3"], subjects(d3))
// assertEquals(39, d3InitialVersion, d3.signal.version)

// a3.set(4)
// assertEquals(41, 9, d3())
// assertEquals(42, ["a3", "c3"], subjects(d3))
// assertEquals(43, d3InitialVersion + 1, d3.signal.version)

// c3.set(5)
// assertEquals(44, 15, d3())
// assertEquals(45, d3InitialVersion + 2, d3.signal.version)

// // Test that changes to b3 DO NOT have any effect over d3 anymore.
// b3.set(6)
// assertEquals(46, 15, d3())
// assertEquals(47, d3InitialVersion + 2, d3.signal.version)

// const a4 = signal("a4", 1)
// const b4 = signal("b4", 2)
// const c4 = signal("c4", 3)
// const d4 = computed("d4", () => a4() === 1 ? b4() * 2 : c4() * 3)
// const e4 = computed("e4", () => d4() * 10)
// const f4 = computed("f4", () => d4() + e4())
// const g4 = computed("g4", () => f4() - d4() * 2, { kind: ComputedSignalKind.Eager })
// const h4 = computed("h4", () => g4() - 2)

// const initialVersions = {
//   a4: a4.signal.version,
//   b4: b4.signal.version,
//   c4: c4.signal.version,
//   d4: d4.signal.version,
//   e4: e4.signal.version,
//   f4: f4.signal.version,
//   g4: g4.signal.version,
//   h4: h4.signal.version
// }

// assertEquals(48, {
//   a4: 1,
//   b4: 1,
//   c4: 1,
//   d4: 1,
//   e4: 1,
//   f4: 1,
//   g4: 1,
//   h4: 0
// },
//   initialVersions
// )

// assertEquals(49, {
//   a4: 1,
//   b4: 2,
//   c4: 3,
//   d4: 4,
//   e4: 40,
//   f4: 44,
//   g4: 36,
//   h4: 34
// }, {
//   a4: a4(), b4: b4(), c4: c4(), d4: d4(), e4: e4(), f4: f4(), g4: g4(), h4: h4()
// })

// a4.set(10)

// assertEquals(50, {
//   a4: initialVersions.a4 + 1,
//   b4: initialVersions.b4,
//   c4: initialVersions.c4,
//   d4: initialVersions.d4 + 1,
//   e4: initialVersions.e4 + 1,
//   f4: initialVersions.f4 + 1,
//   g4: initialVersions.g4 + 1,
//   h4: initialVersions.h4 + 1
// }, {
//   a4: a4.signal.version,
//   b4: b4.signal.version,
//   c4: c4.signal.version,
//   d4: d4.signal.version,
//   e4: e4.signal.version,
//   f4: f4.signal.version,
//   g4: g4.signal.version,
//   h4: h4.signal.version
// })

// assertEquals(51, {
//   a4: 10,
//   b4: 2,
//   c4: 3,
//   d4: 9,
//   e4: 90,
//   f4: 99,
//   g4: 81,
//   h4: 79
// }, {
//   a4: a4(), b4: b4(), c4: c4(), d4: d4(), e4: e4(), f4: f4(), g4: g4(), h4: h4()
// })

// // ----------------------------------------------------------------------------------------------------------

// const EF1_EFFECT_MSG = (previous: number | undefined, current: number) =>
//   `🔥🚀🧨 Hey, firing effect ef1, based on h4... I can see that my previous value was ${previous} (which initially will be my default value) and now is ${current}.`

// let ef1!: IComputedSignalWrapper<number>

// assertDisplays(52,
//   () => {
//       ef1 = effect(
//           "ef1",
//           (prev) => {
//               display(EF1_EFFECT_MSG(prev, h4()))

//               return h4()
//           },
//           999
//       )
//   },
//   EF1_EFFECT_MSG(999, 79)
// )

// assertDisplays(53,
//   () => c4.set(4),
//   EF1_EFFECT_MSG(79, 106)
// )

// assertDisplays(54,
//   () => c4.set(5),
//   EF1_EFFECT_MSG(106, 133)
// )

// assertDisplays(54,
//   () => c4.set(6),
//   EF1_EFFECT_MSG(133, 160)
// )

// assertDisplays(54,
//   () => a4.set(1),
//   EF1_EFFECT_MSG(160, 34)
// )

// inspect(ef1)
// inspect(h4)

// const a5 = computed(
//   "a5",
//   (prev?: number) => a4() + prev! + 1,
//   { defaultValue: 0 }
// )

// log('a4: ', a4())
// log('a5: ', a5())

// a4.set(2)
// log('a4: ', a4())
// log('a5: ', a5())

// // ----------------------------------------------------------------------------------------------------------

// const POWERS_OF_2_TERMS = 10

// // Calculate powersOf2[i] as 2 ** i for N terms, such that fot the ith term, the number of:
// //   - subjects will be: i
// //   - observers will be: (N - 1) - i
// const powersOf2: IComputedSignalWrapper<number>[] = times(
//   POWERS_OF_2_TERMS,
//   i => computed(`powersOf2[${i}]`, () =>
//       powersOf2.slice(0, i).reduce((acc: number, item) => acc + item(), 1)
//   )
// )

// log('Before defining the effect, this proves that NONE of the powersOf2\'s terms have eager behavior: ', powersOf2.some(item => (item.signal.hasEagerBehavior())))

// const e = effect("e", () =>
//   console.log(`powersOf2[${POWERS_OF_2_TERMS - 1}]=`, powersOf2[POWERS_OF_2_TERMS - 1]())
// )

// log('After defining the effect, this confirms that ALL powersOf2\'s terms now have eager behavior: ', powersOf2.every(item => (item.signal.hasEagerBehavior())))
