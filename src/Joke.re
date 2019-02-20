let make = (~joke, ~isFavorite, ~handleClick, _children) => {
  ...ReasonReact.statelessComponent("Joke"),
  render: _self =>
    <div className="columns">
      <div className="column is-three-quarters">
        <div className="content">
          {ReasonReact.string(joke.Service.content)}
        </div>
      </div>
      <div className="column">
        {isFavorite ?
           <a className="delete" onClick={_ => handleClick(joke)} /> :
           <a onClick={_ => handleClick(joke)}>
             {ReasonReact.string("Favorite")}
           </a>}
      </div>
    </div>,
};