[![Build Status](https://bryphe.visualstudio.com/reason-reactify/_apis/build/status/bryphe.reason-reactify)](https://bryphe.visualstudio.com/reason-reactify/_build/latest?definitionId=3)

# Reactify

#### Transform a mutable tree into a functional React-like API, in native Reason!

## Why?

Reason is "react as originally intended", and the language provides excellent faculty for expressing a react-like function API, with built-in JSX support.

Often, when we think of React (or at least when I do) - I think of React in the context of the DOM, but the abstraction is useful in other domains too.

There are often cases where we either inherit some sort of mutable state tree (ie, the DOM), or when we create such a mutable structure for performance reasons (ie, a scene graph in a game engine). Having a functional API that always renders the entire tree is a way to reduce cognitive load, but for the DOM or a scene graph, it'd be too expensive to rebuild it all the time! So a react-like reconciler is useful for being able to express the tree in a stateless, purely functional way, but also reap the performance benefits of only mutating what needs to change.

## Let's build a reconciler!

If you want to skip this stuff, just check out some examples:
- TODO

### Quickstart

The way the library works is you implement a Module that implements some basic functionality:

This will be familiar if you've ever created a React reconciler before!

### Step 1: Tell us

## Development

### Building the project

### Running tests

## License

## Special Thanks


