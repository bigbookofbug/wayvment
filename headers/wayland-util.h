
#ifndef WAYLAND_UTIL_H
#define WAYLAND_UTIL_H

#include <math.h>
#include <stddef.h>
#include <inttypes.h>
#include <stdarg.h>

#ifdef  __cplusplus
extern "C" {
#endif

/** Visibility attribute */
#if defined(__GNUC__) && __GNUC__ >= 4
#define WL_EXPORT __attribute__ ((visibility("default")))
#else
#define WL_EXPORT
#endif

/** Deprecated attribute */
#if __STDC_VERSION__ >= 202311L
#define WL_DEPRECATED [[deprecated]]
#elif defined(__GNUC__) && __GNUC__ >= 4
#define WL_DEPRECATED __attribute__ ((deprecated))
#else
#define WL_DEPRECATED
#endif

/**
 * Printf-style argument attribute
 *
 * \param x Ordinality of the format string argument
 * \param y Ordinality of the argument to check against the format string
 *
 * \sa https://gcc.gnu.org/onlinedocs/gcc-3.2.1/gcc/Function-Attributes.html
 */
#if defined(__GNUC__) && __GNUC__ >= 4
#define WL_PRINTF(x, y) __attribute__((__format__(__printf__, x, y)))
#else
#define WL_PRINTF(x, y)
#endif

#if __STDC_VERSION__ >= 202311L
#define WL_TYPEOF(expr) typeof(expr)
#else
#define WL_TYPEOF(expr) __typeof__(expr)
#endif



/**
 * Initializes the list.
 *
 * \param list List to initialize
 *
 * \memberof wl_list
 */
void
wl_list_init(struct wl_list *list);

/**
 * Inserts an element into the list, after the element represented by \p list.
 * When \p list is a reference to the list itself (the head), set the containing
 * struct of \p elm as the first element in the list.
 *
 * \note If \p elm is already part of a list, inserting it again will lead to
 *       list corruption.
 *
 * \param list List element after which the new element is inserted
 * \param elm Link of the containing struct to insert into the list
 *
 * \memberof wl_list
 */
void
wl_list_insert(struct wl_list *list, struct wl_list *elm);

/**
 * Removes an element from the list.
 *
 * \note This operation leaves \p elm in an invalid state.
 *
 * \param elm Link of the containing struct to remove from the list
 *
 * \memberof wl_list
 */
void
wl_list_remove(struct wl_list *elm);

/**
 * Determines the length of the list.
 *
 * \note This is an O(n) operation.
 *
 * \param list List whose length is to be determined
 *
 * \return Number of elements in the list
 *
 * \memberof wl_list
 */
int
wl_list_length(const struct wl_list *list);

/**
 * Determines if the list is empty.
 *
 * \param list List whose emptiness is to be determined
 *
 * \return 1 if empty, or 0 if not empty
 *
 * \memberof wl_list
 */
int
wl_list_empty(const struct wl_list *list);

/**
 * Inserts all of the elements of one list into another, after the element
 * represented by \p list.
 *
 * \note This leaves \p other in an invalid state.
 *
 * \param list List element after which the other list elements will be inserted
 * \param other List of elements to insert
 *
 * \memberof wl_list
 */
void
wl_list_insert_list(struct wl_list *list, struct wl_list *other);

/**
 * Retrieves a pointer to a containing struct, given a member name.
 *
 * This macro allows "conversion" from a pointer to a member to its containing
 * struct. This is useful if you have a contained item like a wl_list,
 * wl_listener, or wl_signal, provided via a callback or other means, and would
 * like to retrieve the struct that contains it.
 *
 * To demonstrate, the following example retrieves a pointer to
 * `example_container` given only its `destroy_listener` member:
 *
 * \code
 * struct example_container {
 *         struct wl_listener destroy_listener;
 *         // other members...
 * };
 *
 * void example_container_destroy(struct wl_listener *listener, void *data)
 * {
 *         struct example_container *ctr;
 *
 *         ctr = wl_container_of(listener, ctr, destroy_listener);
 *         // destroy ctr...
 * }
 * \endcode
 *
 * \note `sample` need not be a valid pointer. A null or uninitialised pointer
 *       is sufficient.
 *
 * \param ptr Valid pointer to the contained member
 * \param sample Pointer to a struct whose type contains \p ptr
 * \param member Named location of \p ptr within the \p sample type
 *
 * \return The container for the specified pointer
 */
#define wl_container_of(ptr, sample, member)				\
	(WL_TYPEOF(sample))((char *)(ptr) -				\
			     offsetof(WL_TYPEOF(*sample), member))

/**
 * Iterates over a list.
 *
 * This macro expresses a for-each iterator for wl_list. Given a list and
 * wl_list link member name (often named `link` by convention), this macro
 * assigns each element in the list to \p pos, which can then be referenced in
 * a trailing code block. For example, given a wl_list of `struct message`
 * elements:
 *
 * \code
 * struct message {
 *         char *contents;
 *         wl_list link;
 * };
 *
 * struct wl_list *message_list;
 * // Assume message_list now "contains" many messages
 *
 * struct message *m;
 * wl_list_for_each(m, message_list, link) {
 *         do_something_with_message(m);
 * }
 * \endcode
 *
 * \param pos Cursor that each list element will be assigned to
 * \param head Head of the list to iterate over
 * \param member Name of the link member within the element struct
 *
 * \relates wl_list
 */
#define wl_list_for_each(pos, head, member)				\
	for (pos = wl_container_of((head)->next, pos, member);	\
	     &pos->member != (head);					\
	     pos = wl_container_of(pos->member.next, pos, member))

/**
 * Iterates over a list, safe against removal of the list element.
 *
 * \note Only removal of the current element, \p pos, is safe. Removing
 *       any other element during traversal may lead to a loop malfunction.
 *
 * \sa wl_list_for_each()
 *
 * \param pos Cursor that each list element will be assigned to
 * \param tmp Temporary pointer of the same type as \p pos
 * \param head Head of the list to iterate over
 * \param member Name of the link member within the element struct
 *
 * \relates wl_list
 */
#define wl_list_for_each_safe(pos, tmp, head, member)			\
	for (pos = wl_container_of((head)->next, pos, member),		\
	     tmp = wl_container_of((pos)->member.next, tmp, member);	\
	     &pos->member != (head);					\
	     pos = tmp,							\
	     tmp = wl_container_of(pos->member.next, tmp, member))

/**
 * Iterates backwards over a list.
 *
 * \sa wl_list_for_each()
 *
 * \param pos Cursor that each list element will be assigned to
 * \param head Head of the list to iterate over
 * \param member Name of the link member within the element struct
 *
 * \relates wl_list
 */
#define wl_list_for_each_reverse(pos, head, member)			\
	for (pos = wl_container_of((head)->prev, pos, member);	\
	     &pos->member != (head);					\
	     pos = wl_container_of(pos->member.prev, pos, member))

/**
 * Iterates backwards over a list, safe against removal of the list element.
 *
 * \note Only removal of the current element, \p pos, is safe. Removing
 *       any other element during traversal may lead to a loop malfunction.
 *
 * \sa wl_list_for_each()
 *
 * \param pos Cursor that each list element will be assigned to
 * \param tmp Temporary pointer of the same type as \p pos
 * \param head Head of the list to iterate over
 * \param member Name of the link member within the element struct
 *
 * \relates wl_list
 */
#define wl_list_for_each_reverse_safe(pos, tmp, head, member)		\
	for (pos = wl_container_of((head)->prev, pos, member),	\
	     tmp = wl_container_of((pos)->member.prev, tmp, member);	\
	     &pos->member != (head);					\
	     pos = tmp,							\
	     tmp = wl_container_of(pos->member.prev, tmp, member))

/**
 * \class wl_array
 *
 * Dynamic array
 *
 * A wl_array is a dynamic array that can only grow until released. It is
 * intended for relatively small allocations whose size is variable or not known
 * in advance. While construction of a wl_array does not require all elements to
 * be of the same size, wl_array_for_each() does require all elements to have
 * the same type and size.
 *
 */
struct wl_array {
	/** Array size */
	size_t size;
	/** Allocated space */
	size_t alloc;
	/** Array data */
	void *data;
};

/**
 * Initializes the array.
 *
 * \param array Array to initialize
 *
 * \memberof wl_array
 */
void
wl_array_init(struct wl_array *array);

/**
 * Releases the array data.
 *
 * \note Leaves the array in an invalid state.
 *
 * \param array Array whose data is to be released
 *
 * \memberof wl_array
 */
void
wl_array_release(struct wl_array *array);

/**
 * Increases the size of the array by \p size bytes.
 *
 * \param array Array whose size is to be increased
 * \param size Number of bytes to increase the size of the array by
 *
 * \return A pointer to the beginning of the newly appended space, or NULL when
 *         resizing fails.
 *
 * \memberof wl_array
 */
void *
wl_array_add(struct wl_array *array, size_t size);

/**
 * Copies the contents of \p source to \p array.
 *
 * \param array Destination array to copy to
 * \param source Source array to copy from
 *
 * \return 0 on success, or -1 on failure
 *
 * \memberof wl_array
 */
int
wl_array_copy(struct wl_array *array, struct wl_array *source);

/**
 * Iterates over an array.
 *
 * This macro expresses a for-each iterator for wl_array. It assigns each
 * element in the array to \p pos, which can then be referenced in a trailing
 * code block. \p pos must be a pointer to the array element type, and all
 * array elements must be of the same type and size.
 *
 * \param pos Cursor that each array element will be assigned to
 * \param array Array to iterate over
 *
 * \relates wl_array
 * \sa wl_list_for_each()
 */
#define wl_array_for_each(pos, array)					\
	for (pos = (array)->data;					\
	     (array)->size != 0 &&					\
	     (const char *) pos < ((const char *) (array)->data + (array)->size); \
	     (pos)++)

/**
 * Fixed-point number
 *
 * A `wl_fixed_t` is a 24.8 signed fixed-point number with a sign bit, 23 bits
 * of integer precision and 8 bits of decimal precision. Consider `wl_fixed_t`
 * as an opaque struct with methods that facilitate conversion to and from
 * `double` and `int` types.
 */
typedef int32_t wl_fixed_t;

/**
 * Converts a fixed-point number to a floating-point number.
 *
 * \param f Fixed-point number to convert
 *
 * \return Floating-point representation of the fixed-point argument
 */
static inline double
wl_fixed_to_double(wl_fixed_t f)
{
	return f / 256.0;
}

/**
 * Converts a floating-point number to a fixed-point number.
 *
 * \param d Floating-point number to convert
 *
 * \return Fixed-point representation of the floating-point argument
 */
static inline wl_fixed_t
wl_fixed_from_double(double d)
{
	return (wl_fixed_t) (d * 256.0);
}

/**
 * Converts a fixed-point number to an integer.
 *
 * \param f Fixed-point number to convert
 *
 * \return Integer component of the fixed-point argument
 */
static inline int
wl_fixed_to_int(wl_fixed_t f)
{
	return f / 256;
}

/**
 * Converts an integer to a fixed-point number.
 *
 * \param i Integer to convert
 *
 * \return Fixed-point representation of the integer argument
 */
static inline wl_fixed_t
wl_fixed_from_int(int i)
{
	return i * 256;
}

/**
 * Protocol message argument data types
 *
 * This union represents all of the argument types in the Wayland protocol wire
 * format. The protocol implementation uses wl_argument within its marshalling
 * machinery for dispatching messages between a client and a compositor.
 *
 * \sa wl_message
 * \sa wl_interface
 * \sa <a href="https://wayland.freedesktop.org/docs/html/ch04.html#sect-Protocol-wire-Format">Wire Format</a>
 */
union wl_argument {
	int32_t i;           /**< `int`    */
	uint32_t u;          /**< `uint`   */
	wl_fixed_t f;        /**< `fixed`  */
	const char *s;       /**< `string` */
	struct wl_object *o; /**< `object` */
	uint32_t n;          /**< `new_id` */
	struct wl_array *a;  /**< `array`  */
	int32_t h;           /**< `fd`     */
};

/**
 * Dispatcher function type alias
 *
 * A dispatcher is a function that handles the emitting of callbacks in client
 * code. For programs directly using the C library, this is done by using
 * libffi to call function pointers. When binding to languages other than C,
 * dispatchers provide a way to abstract the function calling process to be
 * friendlier to other function calling systems.
 *
 * A dispatcher takes five arguments: The first is the dispatcher-specific
 * implementation associated with the target object. The second is the object
 * upon which the callback is being invoked (either wl_proxy or wl_resource).
 * The third and fourth arguments are the opcode and the wl_message
 * corresponding to the callback. The final argument is an array of arguments
 * received from the other process via the wire protocol.
 *
 * \param user_data Dispatcher-specific implementation data
 * \param target Callback invocation target (wl_proxy or `wl_resource`)
 * \param opcode Callback opcode
 * \param msg Callback message signature
 * \param args Array of received arguments
 *
 * \return 0 on success, or -1 on failure
 */
typedef int (*wl_dispatcher_func_t)(const void *user_data, void *target,
				    uint32_t opcode, const struct wl_message *msg,
				    union wl_argument *args);

/**
 * Log function type alias
 *
 * The C implementation of the Wayland protocol abstracts the details of
 * logging. Users may customize the logging behavior, with a function conforming
 * to the `wl_log_func_t` type, via `wl_log_set_handler_client` and
 * `wl_log_set_handler_server`.
 *
 * A `wl_log_func_t` must conform to the expectations of `vprintf`, and
 * expects two arguments: a string to write and a corresponding variable
 * argument list. While the string to write may contain format specifiers and
 * use values in the variable argument list, the behavior of any `wl_log_func_t`
 * depends on the implementation.
 *
 * \note Take care to not confuse this with `wl_protocol_logger_func_t`, which
 *       is a specific server-side logger for requests and events.
 *
 * \param fmt String to write to the log, containing optional format
 *            specifiers
 * \param args Variable argument list
 *
 * \sa wl_log_set_handler_client
 * \sa wl_log_set_handler_server
 */
typedef void (*wl_log_func_t)(const char *fmt, va_list args) WL_PRINTF(1, 0);

/**
 * Return value of an iterator function
 *
 * \sa wl_client_for_each_resource_iterator_func_t
 * \sa wl_client_for_each_resource
 */
enum wl_iterator_result {
	/** Stop the iteration */
	WL_ITERATOR_STOP,
	/** Continue the iteration */
	WL_ITERATOR_CONTINUE
};

#ifdef  __cplusplus
}
#endif

#endif
