export class SwiProlog {
  bindings = null
  stdin = ''
  stdinPosition = 0

  call(query) {
    const ref = this.bindings.PL_new_term_ref()
    if (!this.bindings.PL_chars_to_term(query, ref))
      throw new Error('Query has a syntax error: ' + query)
    return Boolean(this.bindings.PL_call(ref, 0))
  }

  init(module) {
    this.initBindings(module)

    const argvArray = [
      module.allocate(
        module.intArrayFromString('swipl'),
        'i8',
        module.ALLOC_NORMAL
      ),
      module.allocate(module.intArrayFromString('-x'), 'i8', module.ALLOC_NORMAL),
      module.allocate(
        module.intArrayFromString('wasm-preload/swipl.prc'),
        'i8',
        module.ALLOC_NORMAL
      ),
      module.allocate(
        module.intArrayFromString('--nosignals'),
        'i8',
        module.ALLOC_NORMAL
      ),
    ]

    const argvPtr = module._malloc(argvArray.length * 4)
    for (let i = 0; i < argvArray.length; i++)
      module.setValue(argvPtr + i * 4, argvArray[i], '*')

    if (!this.bindings.PL_initialise(4, argvPtr))
      throw new Error('SWI-Prolog initialisation failed.')

    // Set the path of the preloaded (from swipl-web.dat) standard library.
    // This makes it possible to call use_module(library(lists)) and so on.
    this.call(
      "assert(user:file_search_path(library, 'wasm-preload/library'))."
    )
  };

  initBindings(module) {
    this.bindings = {
      PL_initialise: module.cwrap('PL_initialise', 'number', [
        'number',
        'number',
      ]),
      PL_new_term_ref: module.cwrap('PL_new_term_ref', 'number', []),
      PL_chars_to_term: module.cwrap('PL_chars_to_term', 'number', [
        'string',
        'number',
      ]),
      PL_call: module.cwrap('PL_call', 'number', ['number', 'number']),
    }
  }
  
  query(input) {
    this.setStdin(input)
    // This will execute one iteration of toplevel.
    this.call('break') // see call.js
  }

  readStdin() {
    if (this.stdinPosition >= this.stdin.length) {
      return null;
    } else {
      const code = this.stdin.charCodeAt(this.stdinPosition);
      this.stdinPosition++;
      return code;
    }
  }

  setStdin(string) {
    this.stdin = string;
    this.stdinPosition = 0;
  }
}
