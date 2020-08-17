{logdir, "./logs/"}.
{alias, ft, ".."}.

%% active suites
{suites, ft, [
    common_SUITE,
    producer_SUITE,
    consumer_SUITE
]}.