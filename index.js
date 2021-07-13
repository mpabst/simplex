let session

async function init() {
  let resp = await fetch('index.pl')
  if (!resp.ok) console.error(resp)
  let src = await resp.text()

  session = pl.create()
  session.consult(src, {success: console.log, error: console.error})

  query('process_tick.')
}

// these should all really be async, but ah, fire and foreget for now
function query(q, { success = console.log, error = console.error } = {}) {
  session.query(q, { success, error })
  answer()
}

function answer() {
  session.answer({
    success: console.log,
    error: console.error,
    fail: console.error,
    limit: console.error
  })
}

// function event(ev) {
//   query(`write_event(${ev.target.id}, ${ev.type}).`)
// }

init()
