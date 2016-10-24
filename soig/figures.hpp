#ifndef FIGURES_HPP
#define FIGURES_HPP

#include <vector>

struct Point
{
    int x;
    int y;
    int radius;
};

struct Line
{
    Point p1;
    Point p2;
};

struct Figure
{
    enum Type { POINT, LINE };

    Figure(int x, int y)
        : type(POINT)
    {
        p.x = x;
        p.y = y;
        p.radius = 5;
    }
    Figure(Point x, Point y)
        : type(LINE)
    {
        l.p1 = x;
        l.p2 = y;
    }

    bool IsInside(int x, int y)
    {
        if (type == POINT)
        {
            int left = p.x - p.radius/2;
            int right = p.x + p.radius/2;
            int top = p.y - p.radius/2;
            int bottom = p.y + p.radius/2;

            if (left <= x && x <= right &&
                    top <= y && y <= bottom)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
        else
        {
            return false;
        }
    }

    Type type;
    union
    {
        Point p;
        Line l;
    };


};

typedef std::vector<Figure> FigureList;

#endif // FIGURES_HPP
