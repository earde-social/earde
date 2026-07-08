import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn placeholder_test() {
  let topic = "chan:" <> "8"
  assert topic == "chan:8"
}
