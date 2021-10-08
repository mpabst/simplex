import { Engine } from './engine.js'
import { SwiProlog } from './swi-prolog.js'

const swipl = new SwiProlog()
const engine = new Engine(swipl)

// Stub Module object. Used by swipl-web.js to
// populate the actual Module object.
window.Module = {
  noInitialRun: true,
  locateFile: url => `../dist/${url}`,
  print: console.log,
  printErr: console.error,
  preRun: [() => FS.init(swipl.readStdin.bind(swipl))],
  async onRuntimeInitialized() {
    swipl.init(Module)
    await engine.init()
  }
}
