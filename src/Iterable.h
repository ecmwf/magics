#ifndef	Iterable_h_
#define	Iterable_h_

template<typename Container>
class Iterable
{
		const Container &container;
	public:
		auto begin() const {return container.cbegin();}
		auto end() const {return container.cend();}

		Iterable(const Container &cont): container(cont) {}
		Iterable(Iterable &&) = default;	//allow move construction, suppress move assignment and copy operations
};

#endif	//Iterable_h_
