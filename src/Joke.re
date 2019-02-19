let make = (~content, _children) => {
  ...ReasonReact.statelessComponent("Joke"),
  render: _self =>
    <div className="columns">
      <div className="column is-three-quarters">
        <div className="content"> {ReasonReact.string(content)} </div>
      </div>
      <div className="column">
        <a href="#"> {ReasonReact.string("Favorite")} </a>
      </div>
    </div>,
};