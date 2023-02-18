using MultivariatePolynomials
using DataStructures


function spoly(p,q)
    pq = lcm(leadingmonomial(p),leadingmonomial(q))
    return div(  pq , leadingterm(p) ) * p - div(pq , leadingterm(q)) * q
end

function isgrobner(F::Array{T}) where {T <: AbstractPolynomialLike} # check buchberger criterion
    for (i, f1) in enumerate(F)
        for f2 in F[i+1:end]
            s = spoly(f1,f2)
            _,s = divrem(s,F)
            if !iszero(s)
                return false
            end
        end
    end
    return true
end

function buchberger(F::Array{T}) where {T <: AbstractPolynomialLike}
    pairs = Queue{Tuple{T,T}}()
    # intialize with all pairs from F
    for (i, f1) in enumerate(F)
        for f2 in F[i+1:end]
            enqueue!(pairs, (f1,f2))
        end
    end

    # consider all possible s-polynomials and reduce them
    while !isempty(pairs)
        (f1,f2) = dequeue!(pairs)
        s = spoly(f1,f2)
        _,s = divrem(s,F)
        if !iszero(s) #isapproxzero? Only add to our set if doesn't completely reduce
            for f in F
                enqueue!(pairs, (s,f))
            end
            push!(F,s)
        end
    end

    # reduce redundant entries in grobner basis.
    G = Array{T}(undef, 0)
    while !isempty(F)
        f = pop!(F)
        _,r = divrem(f, vcat(F,G))
        if !iszero(r)
            push!(G,r)
        end
    end
    
    return G
end