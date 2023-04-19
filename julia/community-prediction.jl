# function that replicates the python functionality to recalculate the 
# Metaculus Community prediction. 

using StatsBase

function weighted_median(x, weights, q=0.5)
    wmid = sum(weights) * q

    if wmid <= 0 || any(weights .< 0)
        return NaN
    end

    cmf = cumsum(weights)
    # println("sum cumsum ", sum(cmf))

    ilow = findfirst(cmf .>= wmid) # first value for which cmf is higher than or equal to wmid
    ihigh = findfirst(cmf .> wmid) # first value for which cmf is strictly higher than wmid

    return 0.5 * (x[ilow] + x[ihigh])
end

function calc_latest_cp(predictions)
    n = collect(1:length(predictions))
    w = exp.(sqrt.(n) .- sqrt(length(n)))

    if length(w) <= 2
        w = fill(1, length(w))
    end

    # Make a weighted histogram to get the community prediction
    # 100 breaks. Intervals are closed on the left - intervals are [0.005, 0.015)
    # there are 99 resulting intervals
    bins = range(0.005, 1.0, step=0.01)

    # x gives the mid points for each interval
    x = round.(0.5 .* (bins[2:end] .+ bins[1:(end - 1)]), digits=3)

    # Calculate the weighted histogram
    bin_indices = bin_indices = [searchsortedfirst(bins, p) for p in predictions]
    h2 = zeros(length(bins) - 1)
    for (i, idx) in enumerate(bin_indices)
        if idx > 1 && idx <= length(bins)
            h2[idx - 1] += w[i]
        end
    end

    return round(weighted_median(x, h2, 0.5), digits = 3)
end


using DataFrames

function apply_cp(df)
    df = DataFrame(df)
    grouped_df = groupby(df, :question_id)
    result = combine(grouped_df) do single_df
        n_rows = nrow(single_df)
        cp = Vector{Float64}(undef, n_rows)

        for i in 1:n_rows
            current_df = single_df[1:i, :]
            last_predictions = combine(groupby(current_df, :user_id), :t => (x -> first(findmax(x))) => :t)
            filtered_df = innerjoin(current_df, last_predictions, on = [:user_id, :t])
            cp[i] = calc_latest_cp(filtered_df[:, :prediction])
        end

        single_df[!, "cp"] = cp
        return single_df
    end
    return result
end


# function to remove a single uesr from the dataframe and compute the cp 
# without that user
function remove_user_and_compute_cp(df, user)
    df_without = filter(row -> row.user_id != user, df)
    df_without = apply_cp(df_without)
    insertcols!(df_without, ncol(df_without) + 1, :without_user => user)
    return df_without # select(df_without, [:question_id, :cp, :without_user])
end

# iterates over all unique users, calls the remove_user_and_compute_cp() 
# function for each user, and concatenates the results into a single DataFrame.
function iterate_users_and_compute_cp(df)
    users = unique(df[:, :user_id])
    all_df_without = []

    for user in users
        df_without = remove_user_and_compute_cp(df, user)
        push!(all_df_without, df_without)
    end

    return vcat(all_df_without...)
end


