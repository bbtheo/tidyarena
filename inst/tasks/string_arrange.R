# Sort on a string column; the row order is part of the answer.
list(
  family = "string",
  part = 1L,
  data = "strings",
  code = "customers |> arrange(email, id) |> select(id, email)",
  order_key = "id",
  mem_factor = 1
)
