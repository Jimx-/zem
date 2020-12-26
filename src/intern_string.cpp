#include "intern_string.h"

#include <memory>
#include <unordered_map>

namespace zem {

namespace detail {

using StringKey = std::string;

static std::unordered_map<StringKey,
                          std::shared_ptr<detail::InternStringDetail>>
    intern_pool;

} // namespace detail

detail::InternStringDetail* InternString::intern(const char* str, size_t len)
{
    detail::StringKey key(str, str + len);
    auto iter = detail::intern_pool.find(key);

    if (iter != detail::intern_pool.end()) {
        return iter->second.get();
    }

    std::shared_ptr<detail::InternStringDetail> interned(
        new detail::InternStringDetail(str, len));
    detail::intern_pool[key] = interned;
    return interned.get();
}

} // namespace zem
