#include <stan/math.hpp>
#include <iostream>

int main() {
  std::cout << "log normal(1 | 2, 3)="
            << stan::math::normal_log(1, 2, 3)
            << std::endl;
}
