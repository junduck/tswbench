#pragma once

#include <deque>
#include <functional>
#include <algorithm>

template <class T, class C = std::less<T>, class Allocator = std::allocator<T>>
class sdeque : public std::deque<T, Allocator>
{
private:
    using base = std::deque<T, Allocator>;

public:
    using value_type = typename base::value_type;
    using value_compare = C;
    using allocator_type = typename base::allocator_type;
    using size_type = typename base::size_type;
    using difference_type = typename base::difference_type;
    using reference = typename base::reference;
    using const_reference = typename base::const_reference;
    using pointer = typename base::pointer;
    using const_pointer = typename base::const_pointer;
    using iterator = typename base::iterator;
    using const_iterator = typename base::const_iterator;
    using reverse_iterator = typename base::reverse_iterator;
    using const_reverse_iterator = typename base::const_reverse_iterator;

private:
    value_compare vcmp;

public:
    // Observer

    value_compare value_comp() const
    {
        return value_compare{};
    }

    // Lookup

    size_type count(const value_type &v) const
    {
        const_iterator it0 = std::lower_bound(base::begin(), base::end(), v, vcmp);
        if (it0 == base::end())
        {
            return 0;
        }
        else
        {
            const_iterator it1 = std::lower_bound(it0, base::end(), v, vcmp);
            return std::distance(it0, it1);
        }
    }

    const_iterator find(const value_type &v) const
    {
        const_iterator it = std::lower_bound(base::begin(), base::end(), v, vcmp);
        if (it != base::end() && !vcmp(v, *it))
        {
            return it;
        }
        else
        {
            return base::end();
        }
    }

    iterator find(const value_type &v)
    {
        iterator it = std::lower_bound(base::begin(), base::end(), v, vcmp);
        if (it != base::end() && !vcmp(v, *it))
        {
            return it;
        }
        else
        {
            return base::end();
        }
    }

    const_iterator lower_bound(const value_type &v) const
    {
        return std::lower_bound(base::begin(), base::end(), v, vcmp);
    }

    iterator lower_bound(const value_type &v)
    {
        return std::lower_bound(base::begin(), base::end(), v, vcmp);
    }

    const_iterator upper_bound(const value_type &v) const
    {
        return std::upper_bound(base::begin(), base::end(), v, vcmp);
    }

    iterator upper_bound(const value_type &v)
    {
        return std::upper_bound(base::begin(), base::end(), v, vcmp);
    }

    std::pair<const_iterator, const_iterator> equal_range(const value_type &v) const
    {
        return std::equal_range(base::begin(), base::end(), v, vcmp);
    }

    std::pair<iterator, iterator> equal_range(const value_type &v)
    {
        return std::equal_range(base::begin(), base::end(), v, vcmp);
    }

    // Overridden modifier

    iterator insert(const value_type &v)
    {
        const_iterator it = std::lower_bound(base::begin(), base::end(), v, vcmp);
        return base::insert(it, v);
    }

    iterator insert(value_type &&v)
    {
        const_iterator it = std::lower_bound(base::begin(), base::end(), v, vcmp);
        return base::insert(it, std::move(v));
    }

    void push_front(const value_type &v)
    {
        if (base::empty() || vcmp(v, base::front()))
        {
            base::push_front(v);
        }
        else
        {
            insert(v);
        }
    }

    void push_front(value_type &&v)
    {
        if (base::empty() || vcmp(v, base::front()))
        {
            base::push_front(std::move(v));
        }
        else
        {
            insert(std::move(v));
        }
    }

    void push_back(const value_type &v)
    {
        if (base::empty() || vcmp(base::back(), v))
        {
            base::push_back(v);
        }
        else
        {
            insert(v);
        }
    }

    void push_back(const value_type &&v)
    {
        if (base::empty() || vcmp(base::back(), v))
        {
            base::push_back(std::move(v));
        }
        else
        {
            insert(std::move(v));
        }
    }

    void erase(const value_type &v)
    {
        auto it = std::lower_bound(base::begin(), base::end(), v, vcmp);
        if (it != base::end() && !vcmp(v, *it))
        {
            base::erase(it);
        }
    }

    using base::erase;

    void erase_rank(size_type rank)
    {
        base::erase(base::begin() + rank);
    }

    template <class C2>
    void merge(const sdeque<T, C2, Allocator> &other)
    {
        for (auto it = other.cbegin(); it != other.cend(); ++it)
        {
            insert(*it);
        }
    }

    using base::pop_back;
    using base::pop_front;

    value_type pop()
    {
        value_type tmp{std::move(base::front())};
        base::pop_front();
        return tmp;
    }

private:
    // Hide all none sorted modifiers

    using base::assign;

    using base::emplace;
    using base::emplace_back;
    using base::emplace_front;
    // using base::erase;
    // using base::insert;
    // using base::pop_back;
    // using base::pop_front;
    // using base::push_back;
    // using base::push_front;
    using base::resize;

    // reference access is not hidden
};
