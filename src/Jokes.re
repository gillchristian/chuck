type jokes = list(Service.joke);

type state = {data: Service.remote_data(jokes)};

type action =
  | FetchJokes
  | FetchJokesDone(Service.remote_data(jokes));

let make = _children => {
  ...ReasonReact.reducerComponent("Jokes"),
  initialState: () => {data: Service.NotAsked},
  reducer: (action, _state) =>
    switch (action) {
    | FetchJokes =>
      ReasonReact.UpdateWithSideEffects(
        {data: Service.Loading},
        self =>
          Service.fetch_jokes(10)
          |> Js.Promise.then_(res =>
               (
                 switch (res) {
                 | Service.Ok(d) => Service.Success(d.Service.value)
                 | Service.Fail(e) => Service.Failure(e)
                 }
               )
               |> Js.Promise.resolve
             )
          |> Js.Promise.then_(data =>
               FetchJokesDone(data) |> self.send |> Js.Promise.resolve
             )
          |> ignore,
      )
    | FetchJokesDone(data) => ReasonReact.Update({data: data})
    },
  render: ({send, state}) =>
    <div>
      <div className="section">
        <button
          className={
            Service.is_loading(state.data) ?
              "button is-primary is-loading" : "button is-primary"
          }
          onClick={_ => send(FetchJokes)}
          disabled={Service.is_loading(state.data)}>
          {ReasonReact.string("I want Chuck Norris Jokes!")}
        </button>
      </div>
      <div className="section">
        {switch (state.data) {
         | Service.NotAsked =>
           <div> {ReasonReact.string("What are you waiting for?")} </div>
         | Service.Loading => <div> {ReasonReact.string("Loading...")} </div>
         | Service.Success(jokes) =>
           jokes
           |> List.map(j => <Joke content={j.Service.content} />)
           |> Array.of_list
           |> ReasonReact.array
         | Service.Failure(_) => <div> {ReasonReact.string("Ups ...")} </div>
         }}
      </div>
    </div>,
};