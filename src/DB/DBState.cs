namespace DB
{
    internal enum DBState
    {
        Unknown,
        Connected,
        Closed,
        BeginTransaction,
        Transaction,
        CommitOrRollback,
        TwiceCommitOrRollback

    }
}