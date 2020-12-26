#ifndef _ZEM_SINGLETON_H
#define _ZEM_SINGLETON_H

/* Template class for creating single instance classes */

namespace zem {

template <typename T> class Singleton {
public:
    Singleton() { singleton = static_cast<T*>(this); }

    Singleton(const Singleton<T>&) = delete;
    Singleton<T>& operator=(const Singleton<T>&) = delete;

    ~Singleton() { singleton = nullptr; }

    static T& get_singleton() { return *singleton; }

    static T* get_singleton_ptr() { return singleton; }

protected:
    static T* singleton;
};

} // namespace zem

#endif
