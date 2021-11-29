#include <function-simple.hpp>
#include <iostream>

int main() {
  std::cout << "2 + 2 = " << ex::add(2, 2) << std::endl;
  std::cout << "1 + 4 = " << ex::add(1, 4) << std::endl;
}
