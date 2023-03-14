defmodule Day25 do
  @sample_input """
  1=-0-2
  12111
  2=0=
  21
  2=01
  111
  20012
  112
  1=-1=
  1-12
  12
  1=
  122
  """

  @spec read_input(boolean()) :: [String.t()]
  defp read_input(use_file) do
    raw_input =
      if use_file do
        File.read!("day25-input.txt")
      else
        @sample_input
      end

    raw_input
    |> String.split("\n", trim: true)
  end

  @spec solve(boolean()) :: String.t()
  def solve(use_file) do
    use_file
    |> read_input
    |> Enum.map(&from_snafu/1)
    |> Enum.sum()
    |> to_snafu
    |> elem(1)
  end

  @spec from_snafu(String.t()) :: integer()
  def from_snafu(snafu) do
    snafu
    |> String.split("", trim: true)
    |> Enum.reverse()
    |> Enum.map(&char_to_integer/1)
    |> Enum.with_index()
    |> Enum.reduce(0, fn {val, idx}, acc ->
      val * 5 ** idx + acc
    end)
  end

  @spec char_to_integer(String.t()) :: integer()
  defp char_to_integer(char) do
    case char do
      "2" -> 2
      "1" -> 1
      "0" -> 0
      "-" -> -1
      "=" -> -2
    end
  end

  @spec to_snafu(integer()) :: String.t()
  def to_snafu(decimal) do
    decimal
    |> to_remainders()
    |> Enum.map(&to_snafu_parts/1)
    |> Enum.reduce({0, ""}, fn {carry, number}, {prev_carry, acc_str} ->
      result = number + prev_carry

      if result > 2 do
        {new_carry, new_result} = to_snafu_parts(result)
        {carry + new_carry, to_snafu_numeral(new_result) <> acc_str}
      else
        {carry, to_snafu_numeral(result) <> acc_str}
      end
    end)
  end

  @spec to_remainders(integer()) :: [integer()]
  defp to_remainders(decimal) do
    {result, remainder} = {floor(decimal / 5), rem(decimal, 5)}

    if result == 0 do
      [remainder]
    else
      [remainder | to_remainders(result)]
    end
  end

  @spec to_snafu_parts(integer()) :: {integer(), integer()}
  defp to_snafu_parts(remainder) do
    case remainder do
      0 -> {0, 0}
      1 -> {0, 1}
      2 -> {0, 2}
      3 -> {1, -2}
      4 -> {1, -1}
    end
  end

  @spec to_snafu_numeral(integer()) :: String.t()
  defp to_snafu_numeral(digit) do
    case digit do
      -2 -> "="
      -1 -> "-"
      0 -> "0"
      1 -> "1"
      2 -> "2"
    end
  end
end

answer = Day25.solve(true)
IO.puts("Answer: #{answer}")
# 122-2=200-0111--=200
