#ifndef _APP_H_
#define _APP_H_
#include "Render.hpp"
#include "Controller.hpp"

class App
{
public:
    App()
        : mRenderer()
        , mController(mRenderer)
    {
        pointX = pointY = 100;
    }

    HRESULT Init()
    {
        HRESULT hr = E_FAIL;

        hr = CoInitialize(NULL);

        if (SUCCEEDED(hr))
        {
            hr = CreateClassAndWindow();
        }
        if (SUCCEEDED(hr))
        {
            hr = mRenderer.Init(mhWnd);
        }
        return hr;
    }

    void Run()
    {
        ShowWindow(mhWnd, SW_NORMAL);
        UpdateWindow(mhWnd);

        MSG msg;
        while (GetMessage(&msg, NULL, 0, 0))
        {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }

private:
    static LRESULT CALLBACK WndProc(HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam)
    {
        LRESULT result = 0;

        if (iMsg == WM_CREATE)
        {
            LPCREATESTRUCT pCreateStruct = (LPCREATESTRUCT)lParam;

            SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)(pCreateStruct->lpCreateParams));

            result = 1;
        }
        else
        {
            App* pApp = (App*)GetWindowLongPtr(hwnd, GWLP_USERDATA);
            bool done = false;

            if (pApp)
            {
                switch (iMsg)
                {
                case WM_DISPLAYCHANGE:
                case WM_PAINT:
                {
                    PAINTSTRUCT ps;
                    BeginPaint(hwnd, &ps);
                    pApp->mRenderer.Render();
                    EndPaint(hwnd, &ps);
                }
                    result = 0;
                    done = true;
                    break;
                case WM_LBUTTONDOWN:
                    pApp->OnMouseDown(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam));
                    done = true;
                    result = 0;
                    break;
                case WM_MOUSEMOVE:
                    pApp->OnMouseMove(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam));
                    done = true;
                    result = 0;
                    break;
                case WM_LBUTTONUP:
                    pApp->OnMouseUp(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam));
                    done = true;
                    result = 0;
                    break;
                case WM_KEYDOWN:
                    break;
                case WM_CHAR:
                    pApp->OnChar(static_cast<int>(wParam));
                    done = true;
                    result = 0;
                    break;
                case WM_DESTROY:
                    PostQuitMessage(0);
                    done = true;
                    result = 0;
                    break;
                }
            }

            if (!done)
                return DefWindowProc(hwnd, iMsg, wParam, lParam);
        }

        return result;
    }

    HRESULT CreateClassAndWindow()
    {
        HRESULT hr;

        //mpD2Factory->GetDesktopDpi(&dpiX, &dpiY);

        WNDCLASSEX wc = {};
        wc.cbClsExtra = 0;
        wc.cbSize = sizeof(wc);
        wc.cbWndExtra = sizeof(LONG_PTR);
        wc.hbrBackground = NULL;
        wc.hCursor = LoadCursor(NULL, IDI_APPLICATION);
        wc.hIcon = NULL;
        wc.hInstance = GetModuleHandle(NULL);
        wc.lpfnWndProc = &App::WndProc;
        wc.lpszClassName = _T("App");
        wc.lpszMenuName = NULL;
        wc.style = CS_HREDRAW | CS_VREDRAW;

        ATOM atom = RegisterClassEx(&wc);
        hr = atom ? S_OK : E_FAIL;

        if (SUCCEEDED(hr))
        {
            mhWnd = CreateWindow(
                        _T("App"),
                        _T("App window"),
                        WS_OVERLAPPED,
                        CW_USEDEFAULT,
                        CW_USEDEFAULT,
                        640,
                        480,
                        NULL,
                        NULL,
                        GetModuleHandle(NULL),
                        this
                        );
            hr = mhWnd != NULL ? S_OK : E_FAIL;
        }

        return hr;
    }

    void OnDraw()
    {
        HRESULT hr = S_OK;

        if (SUCCEEDED(hr))
        {
        }
    }

    void OnMouseDown(int x, int y)
    {
        mController.OnMouseDown(x, y);
    }

    void OnMouseMove(int x, int y)
    {
        mController.OnMouseMove(x, y);
    }

    void OnMouseUp(int x, int y)
    {
        mController.OnMouseUp(x, y);
    }

    void OnChar(int ch)
    {
        mController.OnChar(ch);
    }

    HWND mhWnd;
    Renderer mRenderer;
    Controller mController;

    int pointX;
    int pointY;
};
#endif
