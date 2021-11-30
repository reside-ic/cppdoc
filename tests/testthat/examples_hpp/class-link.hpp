namespace ex {
class has_link {
public:
  /// The type used for floating point values
  using real_type = double;

  /// Carry out some calculation
  real_type f();

  /// Additional calculations
  ///
  /// @param x A real_type, possibly from `f()`
  real_type g(real_type x);
}
}
