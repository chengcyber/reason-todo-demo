
type item = {
  id: int,
  title: string,
  completed: bool
};

type state = {
  items: list(item)
};

type action =
  | AddItem(string)
  | ToggleItem(int)
  ;
let str = ReasonReact.stringToElement;

let valueFromEvent = (e): string => (
  e
  |> ReactEventRe.Form.target
  |> ReactDOMRe.domElementToObj
)##value;

module Input = {
  type state = string;
  let component = ReasonReact.reducerComponent("Input");
  let make = (~onSubmit, _children) => {
    ...component,
    initialState: () => "",
    reducer: (newText, _text) => ReasonReact.Update(newText),
    render: ({ state: text, reduce }) =>
      <input
        _type="text"
        value=text
        placeholder="write something to do"
        onChange=(reduce((e) => valueFromEvent(e)))
        onKeyDown=((e) =>
          if (ReactEventRe.Keyboard.key(e) === "Enter") {
            onSubmit(text);
            (reduce( () => "" ))()
          }
        )
      />
  };
};

module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");
  let make = (~item, ~onToggle, _children) => {
    ...component,
    render: (_self) =>
      <div className="item" onClick=((_e) => onToggle()) >
        <input
          _type="checkbox"
          checked=(Js.Boolean.to_js_boolean(item.completed))
        />
        (str(item.title))
      </div>
  };
};

let component = ReasonReact.reducerComponent("TodoApp");

let lastId = ref(0);
let newItem = (text) => {
  lastId := lastId^ + 1;
  { id: lastId^, title: text, completed: true }
};

let make = (_children) => {
  ...component,
  initialState: () => {
    items: [
      { id: 0, title: "write something to do", completed: false }
    ]
  },
  reducer: (action, { items }) =>
    switch action {
    | AddItem(text) => ReasonReact.Update({ items: [newItem(text), ...items] })
    | ToggleItem(id) => {
        let items = List.map(
          item => item.id === id ? { ...item, completed: !item.completed } : item,
          items
        );
        ReasonReact.Update({ items: items })
      }
    }
  ,
  render: ({
    state: {
      items
    },
    reduce
  }) => {
    let num = List.length(items);
    <div className="app">
      <div className="title">
        (str("What to do"))
        <Input
          onSubmit=(reduce((text) => AddItem(text)))
        />
      </div>
      <div className="items">
        (
          List.map( (item) =>
                    <TodoItem
                      key=(string_of_int(item.id))
                      item
                      onToggle=(reduce((_e) => ToggleItem(item.id)))
                    />, items) |> Array.of_list |> ReasonReact.arrayToElement
        )
      </div>
      <div className="footer">
        (str(string_of_int(num) ++ " items" ))
      </div>
    </div>
  }
};
