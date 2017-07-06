#ifndef _CONTROLLER_HPP_
#define _CONTROLLER_HPP_

#include "figures.hpp"
#include "Render.hpp"


class Controller
{
public:
    Controller(Renderer& renderer)
        : mRenderer(renderer)
        , mShouldAddPoint(true)
        , mpDraggingPoint(0)
    {

    }

    void OnMouseDown(int x, int y)
    {
        if (!mpDraggingPoint)
        {
            for (Figure& fig : mFigures                 )
            {
                if (fig.type == Figure::POINT && fig.IsInside(x, y))
                {
                    mpDraggingPoint = &fig;
                    mRenderer.UpdateFigureList(mFigures);
                    break;
                }
            }

        }
    }

    void OnMouseMove(int x, int y)
    {
        if (mpDraggingPoint)
        {
            mpDraggingPoint->p.x = x;
            mpDraggingPoint->p.y = y;
            mRenderer.UpdateFigureList(mFigures);
        }
    }

    void OnMouseUp(int x, int y)
    {
        if (mShouldAddPoint)
        {
            mShouldAddPoint = false;

            AddPoint(x, y);
        }
        else if (mpDraggingPoint)
        {
            mpDraggingPoint = 0;
        }
    }

    void OnChar(int ch)
    {
        char c = (char)ch;
        if (ch == 'a')
        {
            mShouldAddPoint = true;
        }
    }

private:
    void AddPoint(int x, int y)
    {
        mFigures.push_back(Figure(x, y));

        mRenderer.UpdateFigureList(mFigures);
    }

    Renderer& mRenderer;
    bool mShouldAddPoint;
    FigureList mFigures;
    Figure* mpDraggingPoint;
};

#endif
