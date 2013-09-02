#include "TeThreadFunctor.h"
#include "TeAgnostic.h"

  
TeThreadFunctor::TeThreadFunctor()
	: threadReturnValue_(false), threadStartFuncPtr_(0)
{
}


TeThreadFunctor::TeThreadFunctor( TeThreadStartFunctT startFuncPtr )
	: threadReturnValue_(false)
{

    setStartFunctPtr( startFuncPtr  );
}


TeThreadFunctor::~TeThreadFunctor()
{
}

const bool& TeThreadFunctor::getReturnValue() const
{
    return threadReturnValue_;
}

void TeThreadFunctor::setStartFunctPtr( TeThreadStartFunctT startFuncPtr )
{
    TEAGN_TRUE_OR_THROW( ( threadStatus_ == TeThreadStopped ),
                          "Cannot change start function pointer of a running thread" )

	TEAGN_TRUE_OR_THROW( ( startFuncPtr != 0 ),
						 "Invalid thread start function pointer" )

    threadStartFuncPtr_ = startFuncPtr;
}

void TeThreadFunctor::setParameters(const TeThreadParameters& params)
{
	TEAGN_TRUE_OR_THROW( ( threadStatus_ == TeThreadStopped ),
                          "Cannot change thread parameters of a running thread" )

	threadUserParams_ = params;
}

void TeThreadFunctor::run()
{
	TEAGN_TRUE_OR_THROW( ( threadStartFuncPtr_ != 0 ),
						 "Invalid thread start function pointer" )

	threadReturnValue_ = threadStartFuncPtr_(threadUserParams_);
	
	threadUserParams_.clear();
}

