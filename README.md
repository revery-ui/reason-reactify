[![Build Status](https://bryphe.visualstudio.com/reason-reactify/_apis/build/status/bryphe.reason-reactify)](https://bryphe.visualstudio.com/reason-reactify/_build/latest?definitionId=3)

# :rocket: Reactify

#### Transform a mutable tree into a functional React-like API, in native Reason!

## Why?

Reason is "react as originally intended", and the language provides excellent faculty for expressing a react-like function API, with built-in JSX support.

I often think of React purely in the context of web technologies / DOM, but the abstraction is really useful in other domains, too.

There are often cases where we either inherit some sort of mutable state tree (ie, the DOM), or when we create such a mutable structure for performance reasons (ie, a scene graph in a game engine). Having a functional API that always renders the entire tree is a way to reduce cognitive load, but for the DOM or a scene graph, it'd be too expensive to rebuild it all the time. So a react-like reconciler is useful for being able to express the tree in a stateless, purely functional way, but also reap the performance benefits of only mutating what needs to change.

## Let's build a reconciler!

### Quickstart

The way the library works is you implement a Module that implements some basic functionality:

This will be familiar if you've ever created a React reconciler before!

### Step 1: Tell us about your primitives

First, let's pretend we're building a reconciler for a familiar domain - the HTML DOM. (You wouldn't really want to do this for production - you're better off using ReasonReact in that case!). But it's a good example of how a reconciler works end-to-end.

We'll start with an empty module:
```diff
+ module WebReconciler {
+
+ }
```

And we'll create a type `t` that is a [__variant__](https://reasonml.github.io/docs/en/variant) specifying the different types of primitives. Primitives are the _core building blocks_ of your reconciler - these correspond to raw dom nodes, or whatever base type you need for your reconciler.

Let's start it up with a few simple tags:
```diff
module WebReconciler {
+    type t =
+    | Div
+    | Span
+    | Image;
}
```

### Step 2: Tell us your node type

The `node` is the type of the actual object we'll be working with in our mutable state tree. If we're using [`js_of_ocaml`](https://ocsigen.org/js_of_ocaml), we'd get access to the DOM elements via [`Dom_html.element`](https://ocsigen.org/js_of_ocaml/3.1.0/api/Dom_html) - that's the type we'll use for our node.

```diff
module WebReconciler {
    type t =
    | Div
    | Span
    | Image;

+    module Html = Dom_html;
+    type node = Dom_html.element;
}
```

Not too bad so far!

### Step 3: Implement a create function

One of the most important jobs our reconciler has is to turn the _primitive_ objects into real, living _nodes_. Let's implement that now!

```diff
module WebReconciler {
    type t =
    | Div
    | Span
    | Image;

    type node = Dom_html.element;

+    let document = Html.window##.document;
+
+    let createInstance: t => node = (primitive) => {
+        switch(primitive) {
+        | Div => Html.createDiv(document);
+        | Span => Html.createSpan(document);
+        | Image => Html.createImage(document);
+        };
+    };
}
```

Note how easy [pattern matching](https://reasonml.github.io/docs/en/pattern-matching) makes it to go from primitives to nodes.

### Step 4: Implement remaining tree operations 

### Step 5: PROFIT

## Development

### Building the project

### Running tests

## License

## Special Thanks


