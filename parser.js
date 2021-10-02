import { h } from 'https://unpkg.com/snabbdom@3.0.3?module'

const ATOM_CHARS = new Set()

for (let i = 0; i < 26; i++) {
  // numerals
  if (i < 10) ATOM_CHARS.add(String.fromCharCode(48 + i))
  // upper case
  ATOM_CHARS.add(String.fromCharCode(65 + i))
  // lower case
  ATOM_CHARS.add(String.fromCharCode(97 + i))
  ATOM_CHARS.add('_')
}

export const parser = eventHandler => string => {
  const context = [], path = []
  let pos = 0, key, rootNode

  function advance(to) {
    context.pop()
    context.push(to)
  }

  function collectWhile(predicate) {
    let out = ''

    // scan past leading whitespace
    while (string[pos] === ' ') pos++

    while (true) {
      const next = string[pos]
      if (!next) break
      const match = predicate(next)
      // if we've already collected chars into our token, bail immediately
      if (out && !match) break
      out += next
      pos++
      // otherwise, bail only after collecting a single token
      if (!match) break
    }

    return out
  }

  function mode() {
    return context[context.length - 1] ?? 'root'
  }

  function newNode(tagName) {
    return h(tagName, { on: {}, style: {}, props: {} }, [])
  }

  function nextToken() {
    let next = collectWhile(s => ATOM_CHARS.has(s))
    // if we're opening a quote literal, try again
    if (next === '\'') {
      next = collectWhile(s => s !== '\'')
      // advance past closing quote
      pos++
    }
    return next
  }

  function currentNode() {
    return path[path.length - 1]
  }

  function pushNode(node) {
    if (!node) return
    if (!path.length) {
      path.push(node)
      rootNode = node
    } else {
      currentNode().children.push(node)
      if (typeof node !== 'string') path.push(node)
    }
  }

  function popNode() {
    const curr = currentNode()
    // snabbdom expects nodes with a single text child to use the `text` prop
    // instead of `children`; we do this munging here because we know that by
    // now we've seen all of this node's children
    if (curr.children.length === 1 && typeof curr.children[0] === 'string') {
      curr.text = curr.children[0]
      curr.children = undefined
    }
    path.pop()
  }

  const rules = {
    root(next) {
      switch (next) {
        case 'h':
          context.push('node')
          break
        default: throw next
      }
    },

    node(next) {
      switch (next) {
        case '(': break
        case ')':
          popNode()
          context.pop()
          break
        default:
          pushNode(newNode(next))
          context.push('afterTagName')
      }
    },

    afterTagName(next) {
      switch (next) {
        case ',': break
        case '[':
          advance('propList')
          break
        default: throw next
      }
    },

    propList(next) {
      switch (next) {
        case ',': break
        case ']':
          advance('afterProps')
          break
        default:
          key = next
          context.push('afterKey')
      }
    },

    afterKey(next) {
      switch (next) {
        case '(': break
        default: 
          if (key === 'on') currentNode().data.on[next] = eventHandler
          else currentNode().data.props[key] = next
          advance('afterVal')
      }
    },

    afterVal(next) {
      switch (next) {
        case ')':
          context.pop()
          break
        default: throw next
      }
    },

    afterProps(next) {
      switch (next) {
        case ',': break
        case '[':
          advance('children')
          break
        default: throw next
      }
    },

    children(next) {
      switch (next) {
        case 'h':
          context.push('node')
          break
        case ',': break
        case ']':
          context.pop()
          break
        // text node
        default: pushNode(next)
      }
    },
  }

  while (true) {
    const next = nextToken()
    if (!next) return rootNode
    rules[mode()](next)
  }
}
