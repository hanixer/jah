#ifndef _RENDER_HPP_
#define _RENDER_HPP_
#include "Common.hpp"
#include "figures.hpp"

class Renderer
{
public:
    Renderer()
        : mhWnd(0)
        , mpD2Factory(0)
        , mpRenderTarget(0)

    {

    }

    HRESULT Init(HWND hwnd)
    {
        mhWnd = hwnd;
        return D2D1CreateFactory(
                    D2D1_FACTORY_TYPE_SINGLE_THREADED,
                    {D2D1_DEBUG_LEVEL_NONE},
                    &mpD2Factory);
    }

    void Render()
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
                mpRenderTarget->DrawLine(D2D1::Point2F(
                                             static_cast<FLOAT>(i), 0.0f),
                                         D2D1::Point2F(static_cast<FLOAT>(i), static_cast<FLOAT>(size.height)),
                                         mpGreyBrush, 0.5f);
            }

            for (int i = 0; i < height; i += 10)
            {
                mpRenderTarget->DrawLine(D2D1::Point2F(0, i),
                                         D2D1::Point2F(size.width, i), mpBlueBrush, 0.5f);
            }

            for (const Figure& fig : mFigures)
            {
                if (fig.type == Figure::POINT)
                {
                    mpRenderTarget->FillEllipse(
                                D2D1::Ellipse(
                                    D2D1::Point2F(
                                        fig.p.x, fig.p.y), fig.p.radius, fig.p.radius), mpBlackBrush);
                }
            }

            hr = mpRenderTarget->EndDraw();
            if (hr == D2DERR_RECREATE_TARGET)
            {
                hr = S_OK;
                DiscardDevicesResources();
            }
        }
    }

    void SetHwnd(HWND hwnd) { mhWnd = hwnd; }

    void UpdateFigureList(const FigureList& figures)
    {
        mFigures = figures;

        Repaint();
    }

private:
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
            if (SUCCEEDED(hr))
            {
                hr = mpRenderTarget->CreateSolidColorBrush(
                            D2D1::ColorF(D2D1::ColorF::Black),
                            &mpBlackBrush);
            }
        }
        return hr;
    }

    void DiscardDevicesResources()
    {
        SafeRelease(&mpRenderTarget);
    }

    void Repaint()
    {
        InvalidateRect(mhWnd, NULL, true);
    }

    HWND mhWnd;
    ID2D1Factory* mpD2Factory;
    ID2D1HwndRenderTarget* mpRenderTarget;
    ID2D1SolidColorBrush* mpBlackBrush;
    ID2D1SolidColorBrush* mpGreyBrush;
    ID2D1SolidColorBrush* mpBlueBrush;
    FigureList mFigures;
};

#endif
