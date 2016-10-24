#include <functional>
#include <Windows.h>
#include <WindowsX.h>
#include <d2d1.h>
#include <tchar.h>
#include <vector>
#include <string>

#pragma comment(lib, "d2d1.lib")

template <typename Interface>
void SafeRelease(Interface** ppInterface)
{
	if (ppInterface && *ppInterface)
	{
		(*ppInterface)->Release();
		*ppInterface = NULL;
	}
}

class App
{
public:
	App()
		: mpD2Factory(0)
		, mpRenderTarget(0)
	{
		pointX = pointY = 100;
	}

	HRESULT Init()
	{
		HRESULT hr = E_FAIL;

		if (SUCCEEDED(CoInitialize(NULL)))
		{
			D2D1_FACTORY_OPTIONS factoryOpts = {D2D1_DEBUG_LEVEL_NONE};
			hr = D2D1CreateFactory(
				D2D1_FACTORY_TYPE_SINGLE_THREADED,
				factoryOpts, 
				&mpD2Factory);
		}
		if (SUCCEEDED(hr))
		{
			hr = CreateClassAndWindow();
		}
		if (SUCCEEDED(hr))
		{
			//hr = CreateDeviceResources();
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
						pApp->OnDraw();
						EndPaint(hwnd, &ps);
					}
					result = 0;
					done = true;
					break;
				case WM_LBUTTONUP:
					pApp->OnButtonUp(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam));
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
		FLOAT dpiX, dpiY;
		HRESULT hr;

		mpD2Factory->GetDesktopDpi(&dpiX, &dpiY);

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

	HRESULT CreateDeviceResources()
	{
		HRESULT hr = S_OK;
		if (!mpRenderTarget)
		{

			RECT rect;
			GetClientRect(mhWnd, &rect);
			D2D1_SIZE_U size = D2D1::SizeU(rect.right - rect.left, rect.bottom - rect.top);

			hr = mpD2Factory->CreateHwndRenderTarget(
				D2D1::RenderTargetProperties(),
				D2D1::HwndRenderTargetProperties(mhWnd, size),
				&mpRenderTarget);
			if (SUCCEEDED(hr))
			{
				hr = mpRenderTarget->CreateSolidColorBrush(
					D2D1::ColorF(D2D1::ColorF::Gray),
					&mpGreyBrush);
			}
			if (SUCCEEDED(hr))
			{
				hr = mpRenderTarget->CreateSolidColorBrush(
					D2D1::ColorF(D2D1::ColorF::Blue),
					&mpBlueBrush);
			}
		}
		return hr;
	}

	void DiscardDeviceReosurces()
	{
		SafeRelease(&mpRenderTarget);
		SafeRelease(&mpGreyBrush);
		SafeRelease(&mpBlueBrush);
	}

	void OnDraw()
	{
		HRESULT hr = S_OK;
		hr = CreateDeviceResources();

		if (SUCCEEDED(hr))
		{
			mpRenderTarget->BeginDraw();

			mpRenderTarget->SetTransform(D2D1::Matrix3x2F::Identity());
			mpRenderTarget->Clear(D2D1::ColorF(D2D1::ColorF::White));

			D2D1_SIZE_F size = mpRenderTarget->GetSize();
			int width = static_cast<int>(size.width);
			int height = static_cast<int>(size.height);

			for (int i = 0; i < width; i += 30)
			{
				mpRenderTarget->DrawLine(D2D1::Point2F(i, 0), 
					D2D1::Point2F(i, size.height), mpGreyBrush, 0.5f);
			}

			for (int i = 0; i < height; i += 10)
			{
				mpRenderTarget->DrawLine(D2D1::Point2F(0, i), 
					D2D1::Point2F(size.width, i), mpBlueBrush, 0.5f);
			}

			static int x = 0;
			static int y = 50;

			mpRenderTarget->DrawLine(
				D2D1::Point2F(0, y),
				D2D1::Point2F(x++, y),
				mpBlueBrush, 10.0f);

			FLOAT radius = 1;
			D2D1_ELLIPSE ellipse = {{pointX, pointY}, radius, radius};
			mpRenderTarget->DrawEllipse(ellipse, mpGreyBrush, 1.0);
			mpRenderTarget->FillEllipse(ellipse, mpBlueBrush);

			hr = mpRenderTarget->EndDraw();
			if (hr == D2DERR_RECREATE_TARGET)
			{
				hr = S_OK;
				DiscardDeviceReosurces();
			}
		}
	}

	void OnButtonUp(int x, int y)
	{
		pointX = x;
		pointY = y;

		InvalidateRect(mhWnd, NULL, TRUE);
	}

	HWND mhWnd;
	ID2D1Factory* mpD2Factory;
	ID2D1HwndRenderTarget* mpRenderTarget;
	ID2D1SolidColorBrush* mpGreyBrush;
	ID2D1SolidColorBrush* mpBlueBrush;

	int pointX;
	int pointY;
};

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