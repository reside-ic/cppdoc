namespace ex {

/// Do a thing, but generically
/// @tparam T The first type
/// @tparam U The second type
/// @param x The first argument
/// @param y The second argument
/// @return A thing of the same type as the first argument
template <typename T, typename U>
T generic(const T& x, U y);

}
