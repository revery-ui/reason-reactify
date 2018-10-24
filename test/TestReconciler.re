
/**
 * Implementation of a very simple reconciler,
 * useful for testing the basic functionality
*/

type recordType = {
    testVal: int
};

type primitives = 
    | Root
    | A(int)
    | B
    | C
    | D(recordType);

/*
 * Keep a record of the updates - we'll use this for certain kinds of tests
 */
type updateType =
    | Append
    | Create
    | Remove
    | Update
    | Replace

let updates: ref(list(updateType)) = ref([]);

let printUpdate = (u) => {
    switch (u) {
    | Append => print_endline("- append");
    | Create => print_endline("- create");
    | Remove => print_endline("- remove");
    | Update => print_endline("- update");
    | Replace => print_endline("- replace");
    };
};

type node = {
    children: ref(list(node)),
    nodeType: primitives,
};

let getUpdates = () => {
    updates^
};

let pushUpdate = (u) => {
    updates := List.append(getUpdates(), [u]);
    printUpdate(u);
};

let createInstance = (prim) => {
    print_endline ("create instance called");
    let ret = {
        children: ref([]),
        nodeType: prim
    };
    pushUpdate(Create);
    ret;
};

let appendChild = (parent, child) => {
    parent.children := parent.children^ @ [child];
    print_endline("append child - new count: " ++ string_of_int(List.length(parent.children^)));
    pushUpdate(Append);
};

let removeChild = (parent, child) => {
    parent.children := List.filter((c) => c != child, parent.children^);
    pushUpdate(Remove);
};

let updateInstance = () => {
    pushUpdate(Update);
};

let clearUpdates = () => {
    updates := [];
};

let printUpdates = () => {
    List.iter(printUpdate, getUpdates());
};

let replaceChild = (parent, newChild, oldChild) => {
    removeChild(parent, oldChild);
    appendChild(parent, newChild);
    pushUpdate(Replace);
};


let a11 = A(1);
let a12 = A(1);
let a2 = A(2);

let d11 = D({testVal: 1});
let d12 = D({testVal: 1});
let d2 = D({testVal: 2});

let equal_constructors = (x: 'a, y: 'a) => {
  let r = Obj.repr(x)
  and s = Obj.repr(y);
  if (Obj.is_int(r) && Obj.is_int(s)) {
    (Obj.obj(r): int) == (Obj.obj(s): int);
  } else if (Obj.is_block(r) && Obj.is_block(s)) {
    Obj.tag(r) == Obj.tag(s);
  } else {
    false;
  };
};

    print_endline ("a11 == a12? " ++ string_of_bool(a11 == a12));
    print_endline ("a12 == a2?" ++ string_of_bool(a2 == a11));
    print_endline("equal constructors a11 & a2?" ++ string_of_bool(equal_constructors(a11, a2)));
    print_endline("equal constructors b & c?" ++ string_of_bool(equal_constructors(B, C)));

    print_endline ("d11 == d12? " ++ string_of_bool(d11 == d12));
    print_endline ("d12 == d2?" ++ string_of_bool(d2 == d11));
    print_endline ("equal constructors d & d?" ++ string_of_bool(equal_constructors(d11, d2)));
