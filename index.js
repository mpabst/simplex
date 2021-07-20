import { h } from 'https://unpkg.com/snabbdom@3.0.3?module'

let session

async function init() {
  let resp = await fetch('index.pl')
  if (!resp.ok) console.error(resp)
  let src = await resp.text()

  session = pl.create()
  await consult(src)
  await tick()
}

async function answer() {
  return new Promise((resolve, reject) => {
    session.answer({
      success: resolve,
      error: reject,
      fail: reject,
      limit: reject
    })
  })
}

async function consult(src) {
  return new Promise((success, error) => {
    session.consult(src, { success, error })
  })
}

async function event(ev) {
  await query(`write_event(${ev.target.id}, ${ev.type}).`)
  await tick()
}

async function query(q) {
  return new Promise((success, error) => session.query(q, { success , error }))
}

async function tick() {
  await query('process_tick(Html).')
  let html = await answer()
  console.log(html)
}

init()
