struct alignas(8) S {};
int main() {
    bool a;
    wchar_t b;
    char16_t c;
    achar32_t d;
    std::cout << alignof(char);
}

#define
#if
_Pragma
#elif

#define FUNCTION(name, a) int fun_##name() { return a;}

#pragma once 1
#pragma pack
#pragma STDC CX_LIMITED_RANGE

sizeof(1)
sizeof...(1)

i = 1;
i = NULL;
i = INFINI;

auto y = [&r = x, x = x + 1, *this]()->int
    {
        r += 2;
        return x + 2;
    }(); // updates ::x to 6 and initializes y to 7.