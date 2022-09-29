#include <iostream>

extern "C" {
    void extra_print(int x) {
        std::cout << x << std::endl;
    }
}
