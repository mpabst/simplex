import { Engine } from './engine.js'
import { SwiProlog } from './swi-prolog.js'

const swipl = new SwiProlog()
const engine = new Engine(swipl)

function print(line) {
  console.log(line)
  const match = line.match(/^Html = (.+)$/)
  if (match) {
    const newVDOM = engine.parse(match[1])
    console.log(newVDOM)
  } else console.log(line)
}

// Stub Module object. Used by swipl-web.js to
// populate the actual Module object.
window.Module = {
  noInitialRun: true,
  locateFile: url => `../dist/${url}`,
  print: line => engine.render(line),
  printErr: console.error,
  preRun: [() => FS.init(swipl.readStdin.bind(swipl))],
  async onRuntimeInitialized() {
    swipl.init(Module)
    await engine.init()
  }
}
