#ifndef _COMMON_HPP_
#define _COMMON_HPP_

#include <Windows.h>
#include <WindowsX.h>
#include <d2d1.h>
#include <tchar.h>

template <typename Interface>
void SafeRelease(Interface** ppInterface)
{
    if (ppInterface && *ppInterface)
    {
        (*ppInterface)->Release();
        *ppInterface = NULL;
    }
}

#endif
