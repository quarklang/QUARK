#ifndef PRETTY_PRINT_STL
#define PRETTY_PRINT_STL

#include <iostream>
#include <iterator>
#include <type_traits>
#include <vector>
#include <algorithm>

// This works similar to ostream_iterator, but doesn't print a delimiter after the final item
template<typename T, typename TChar = char, typename TCharTraits = std::char_traits<TChar> >
class pretty_ostream_iterator : public std::iterator<std::output_iterator_tag, void, void, void, void>
{
public:
    typedef TChar char_type;
    typedef TCharTraits traits_type;
    typedef std::basic_ostream<TChar, TCharTraits> ostream_type;

    pretty_ostream_iterator(ostream_type &stream, const char_type *delim = NULL)
        : _stream(&stream), _delim(delim), _insertDelim(false)
    {
    }

    pretty_ostream_iterator<T, TChar, TCharTraits>& operator=(const T &value)
    {
        if( _delim != NULL )
        {
            // Don't insert a delimiter if this is the first time the function is called
            if( _insertDelim )
                (*_stream) << _delim;
            else
                _insertDelim = true;
        }
        (*_stream) << value;
        return *this;
    }

    pretty_ostream_iterator<T, TChar, TCharTraits>& operator*()
    {
        return *this;
    }

    pretty_ostream_iterator<T, TChar, TCharTraits>& operator++()
    {
        return *this;
    }

    pretty_ostream_iterator<T, TChar, TCharTraits>& operator++(int)
    {
        return *this;
    }
private:
    ostream_type *_stream;
    const char_type *_delim;
    bool _insertDelim;
};

#if _MSC_VER >= 1400

// Declare pretty_ostream_iterator as checked
template<typename T, typename TChar, typename TCharTraits>
struct std::_Is_checked_helper<pretty_ostream_iterator<T, TChar, TCharTraits> > : public std::tr1::true_type
{
};

#endif // _MSC_VER >= 1400

namespace std
{
    // Pre-declarations of container types so we don't actually have to include the relevant headers if not needed, speeding up compilation time.
    // These aren't necessary if you do actually include the headers.
    template<typename T, typename TAllocator> class vector;
    template<typename T, typename TAllocator> class list;
    template<typename T, typename TTraits, typename TAllocator> class set;
    template<typename TKey, typename TValue, typename TTraits, typename TAllocator> class map;
}

// Basic is_container template; specialize to derive from std::true_type for all desired container types
template<typename T> struct is_container : public std::false_type { };

// Mark vector as a container
template<typename T, typename TAllocator> struct is_container<std::vector<T, TAllocator> > : public std::true_type { };

// Mark list as a container
template<typename T, typename TAllocator> struct is_container<std::list<T, TAllocator> > : public std::true_type { };

// Mark set as a container
template<typename T, typename TTraits, typename TAllocator> struct is_container<std::set<T, TTraits, TAllocator> > : public std::true_type { };

// Mark map as a container
template<typename TKey, typename TValue, typename TTraits, typename TAllocator> struct is_container<std::map<TKey, TValue, TTraits, TAllocator> > : public std::true_type { };

// Holds the delimiter values for a specific character type
template<typename TChar>
struct delimiters_values
{
    typedef TChar char_type;
    const TChar *prefix;
    const TChar *delimiter;
    const TChar *postfix;
};

// Defines the delimiter values for a specific container and character type
template<typename T, typename TChar>
struct delimiters
{
    static const delimiters_values<TChar> values; 
};

// Default delimiters
template<typename T> struct delimiters<T, char> { static const delimiters_values<char> values; };
template<typename T> const delimiters_values<char> delimiters<T, char>::values = { "[", ", ", "]" };
template<typename T> struct delimiters<T, wchar_t> { static const delimiters_values<wchar_t> values; };
template<typename T> const delimiters_values<wchar_t> delimiters<T, wchar_t>::values = { L"[", L", ", L"]" };

// Delimiters for set
template<typename T, typename TTraits, typename TAllocator> struct delimiters<std::set<T, TTraits, TAllocator>, char> { static const delimiters_values<char> values; };
template<typename T, typename TTraits, typename TAllocator> const delimiters_values<char> delimiters<std::set<T, TTraits, TAllocator>, char>::values = { "[ ", ", ", " ]" };
template<typename T, typename TTraits, typename TAllocator> struct delimiters<std::set<T, TTraits, TAllocator>, wchar_t> { static const delimiters_values<wchar_t> values; };
template<typename T, typename TTraits, typename TAllocator> const delimiters_values<wchar_t> delimiters<std::set<T, TTraits, TAllocator>, wchar_t>::values = { L"[ ", L", ", L" ]" };

// Delimiters for pair
template<typename T1, typename T2> struct delimiters<std::pair<T1, T2>, char> { static const delimiters_values<char> values; };
template<typename T1, typename T2> const delimiters_values<char> delimiters<std::pair<T1, T2>, char>::values = { "(", ", ", ")" };
template<typename T1, typename T2> struct delimiters<std::pair<T1, T2>, wchar_t> { static const delimiters_values<wchar_t> values; };
template<typename T1, typename T2> const delimiters_values<wchar_t> delimiters<std::pair<T1, T2>, wchar_t>::values = { L"(", L", ", L")" };

// Functor to print containers. You can use this directly if you want to specificy a non-default delimiters type.
template<typename T, typename TChar = char, typename TCharTraits = std::char_traits<TChar>, typename TDelimiters = delimiters<T, TChar> >
struct print_container_helper
{
    typedef TChar char_type;
    typedef TDelimiters delimiters_type;
    typedef std::basic_ostream<TChar, TCharTraits>& ostream_type;

    print_container_helper(const T &container)
        : _container(&container)
    {
    }

    void operator()(ostream_type &stream) const
    {
        if( delimiters_type::values.prefix != NULL )
            stream << delimiters_type::values.prefix;
        std::copy(_container->begin(), _container->end(), pretty_ostream_iterator<typename T::value_type, TChar, TCharTraits>(stream, delimiters_type::values.delimiter));
        if( delimiters_type::values.postfix != NULL )
            stream << delimiters_type::values.postfix;
    }
private:
    const T *_container;
};

// Prints a print_container_helper to the specified stream.
template<typename T, typename TChar, typename TCharTraits, typename TDelimiters>
std::basic_ostream<TChar, TCharTraits>& operator<<(std::basic_ostream<TChar, TCharTraits> &stream, const print_container_helper<T, TChar, TDelimiters> &helper)
{
    helper(stream);
    return stream;
}

// Prints a container to the stream using default delimiters
template<typename T, typename TChar, typename TCharTraits>
typename std::enable_if<is_container<T>::value, std::basic_ostream<TChar, TCharTraits>&>::type
    operator<<(std::basic_ostream<TChar, TCharTraits> &stream, const T &container)
{
    stream << print_container_helper<T, TChar, TCharTraits>(container);
    return stream;
}

// Prints a pair to the stream using delimiters from delimiters<std::pair<T1, T2>>.
template<typename T1, typename T2, typename TChar, typename TCharTraits>
std::basic_ostream<TChar, TCharTraits>& operator<<(std::basic_ostream<TChar, TCharTraits> &stream, const std::pair<T1, T2> &value)
{
    if( delimiters<std::pair<T1, T2>, TChar>::values.prefix != NULL )
        stream << delimiters<std::pair<T1, T2>, TChar>::values.prefix;

    stream << value.first;

    if( delimiters<std::pair<T1, T2>, TChar>::values.delimiter != NULL )
        stream << delimiters<std::pair<T1, T2>, TChar>::values.delimiter;

    stream << value.second;

    if( delimiters<std::pair<T1, T2>, TChar>::values.postfix != NULL )
        stream << delimiters<std::pair<T1, T2>, TChar>::values.postfix;
    return stream;    
}

#endif
