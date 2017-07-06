#include "Common.hpp"
#include "App.hpp"
#include <functional>
#include <vector>
#include <string>

#pragma comment(lib, "d2d1.lib")





int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hOld, LPSTR pCmdLine, int cmdShow)
{
    App app;
    if (SUCCEEDED(app.Init()))
    {
        app.Run();
    }
    else
    {
        OutputDebugString(_T("Failed to initizlied\n"));
    }
    return 0;
}
