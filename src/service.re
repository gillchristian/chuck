/* decoding */
type joke = {
  id: int,
  content: string,
};

type response = {value: list(joke)};

let decode_joke = (json): joke => {
  id: Json.Decode.field("id", Json.Decode.int, json),
  content: Json.Decode.field("joke", Json.Decode.string, json),
};

let decode_jokes = Json.Decode.list(decode_joke);

let decode_response = json => {
  value: Json.Decode.field("value", decode_jokes, json),
};

/* encoding */

let encode_joke = joke =>
  Json.Encode.object_([
    ("id", Json.Encode.int(joke.id)),
    ("joke", Json.Encode.string(joke.content)),
  ]);

let encode_jokes = jokes => Json.Encode.list(encode_joke, jokes);

/* TODO: failure decoder */
let decore_error_msg = json =>
  Json.Decode.field("error", Json.Decode.string, json);

/* fetch */

type result('a, 'b) =
  | Ok('a)
  | Fail('b);

let something_went_wrong = _err =>
  Fail("Something went wront...") |> Js.Promise.resolve;

let handle_response = res => {
  Js.Promise.(
    Fetch.Response.(
      if (ok(res)) {
        res |> json |> then_(j => resolve(Ok(j)));
      } else {
        res |> json |> then_(j => resolve(Fail(j)));
      }
    )
  );
};

let fetch = (url, success_decoder) =>
  Js.Promise.(
    Fetch.fetch(url)
    |> then_(handle_response)
    |> then_(either_json =>
         resolve(
           switch (either_json) {
           | Ok(res) => Ok(success_decoder(res))
           | Fail(json) => Fail(decore_error_msg(json))
           },
         )
       )
    |> catch(something_went_wrong)
  );

/* fetch methods */

let fetch_jokes = count => {
  let url = "http://api.icndb.com/jokes/random/" ++ string_of_int(count);

  fetch(url, decode_response);
};