/*
 * Interface to bridge a React-style functional API
 * with a mutable back-end. Similiar in spirit
 * to the reconciler interface provided via 'react-conciler'.
 */

module type Reconciler = {
  /*
      Primitives is a variant type describing metadata needed
      to instantiate something in the mutable world.
   */
  type primitives;

  /*
      A node is a live instance representing a mutable object
   */
  type node;

  let appendChild: (node, node) => unit;

  let createInstance: primitives => node;

  let replaceChild: (node, node, node) => unit;

  let removeChild: (node, node) => unit;

  let updateInstance: (node, primitives, primitives) => unit;
};

module type React = {
  type primitives;
  type node;
  type hook('t);
  type state('a);

  type renderedElement =
    | RenderedPrimitive(node)
  and elementWithChildren = (list(element), Effects.effects, Context.t)
  and render = unit => elementWithChildren
  and element =
    | Primitive(primitives, render)
    | Component(ComponentId.t, render)
    | Provider(render)
    | Empty(render)
  and elementHook = (hook(unit), element);

  type t;

  /*
       Container API
   */
  type reconcileNotification = node => unit;
  let createContainer:
    (
      ~onBeginReconcile: reconcileNotification=?,
      ~onEndReconcile: reconcileNotification=?,
      node
    ) =>
    t;
  let updateContainer: (t, elementHook) => unit;

  /*
       Component creation API
   */
  let primitiveComponent:
    (~children: list(elementHook), primitives) => elementHook;

  module type Component = {
    type hooks;
    type createElement;
    let createElement: createElement;
  };

  let createComponent:
    (
      (
        (unit => ('h, element), ~children: list(elementHook)) => elementHook
      ) =>
      'c
    ) =>
    (module Component with type createElement = 'c and type hooks = 'h);

  /*
       Component API
   */

  type providerConstructor('t) =
    (~children: list(elementHook), ~value: 't, unit) => elementHook;
  type context('t);

  let getProvider: context('t) => providerConstructor('t);
  let createContext: 't => context('t);
  let useContext: context('t) => 't;

  let empty: elementHook;

  let useEffect:
    (~condition: Effects.effectCondition=?, Effects.effectFunction) => unit;

  let useState:
    ('state, ('state, 'state => unit) => (hook('a), 'b)) =>
    (hook(('a, state('state))), 'b);

  let useReducer:
    (('state, 'action) => 'state, 'state) => ('state, 'action => unit);
};