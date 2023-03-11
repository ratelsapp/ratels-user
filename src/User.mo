import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import List "mo:base/List";
import Option "mo:base/Option";
import Order "mo:base/Order";
import Int "mo:base/Int";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Buffer "mo:base/Buffer";
import Char "mo:base/Char";
import ExperimentalCycles "mo:base/ExperimentalCycles";
import Debug "mo:base/Debug";
import Prim "mo:⛔";
import PrincipalUtils "mo:ic-commons/PrincipalUtils";
import CollectUtils "mo:ic-commons/CollectUtils";

actor {
    type User = {
        user : Principal;
        account : Text;
        principalId : Text;
        twitter : ?Text;
        twitterTime : ?Int;
        discord : ?Text;
        discordTime : ?Int;
        github : ?Text;
        githubTime : ?Int;
    };

    type VerifyType = {
        #twitter;
        #discord;
        #github;
    };

    type UserAccount = {
        user : Principal;
        code : Text;
        nickname : Text;
        following : [Principal];
        followers : [Principal];
    };

    type UserAccountResponse = {
        user : Principal;
        code : Text;
        nickname : Text;
        following : Nat;
        followers : Nat;
    };

    private stable var userArray : [(Text, User)] = [];
    private var users : HashMap.HashMap<Text, User> = HashMap.HashMap<Text, User>(1, Text.equal, Text.hash);
    private stable var userAccountArray : [(Text, UserAccount)] = [];
    private var userAccounts : HashMap.HashMap<Text, UserAccount> = HashMap.HashMap<Text, UserAccount>(1, Text.equal, Text.hash);

    private stable var codeIndex : Nat = 1;
    private var codeLength : Nat = 6;

    private func arrayAdd<T>(array: [T], item: T) : [T] {
        var newArray : Buffer.Buffer<T> = Buffer.Buffer<T>(0);
        newArray.add(item);
        for (t : T in array.vals()) {
            newArray.add(t);
        };
        return newArray.toArray();
    };

    private func createUserCode() : Text {
        var code: Text = Nat.toText(codeIndex);
        codeIndex += 1;
        while (code.size() < codeLength) {
            code := "0" # code;
        };
        code
    };

    private func getUserAccount(caller: Principal) : UserAccount {
        let userAddress: Text = PrincipalUtils.toAddress(caller);
        switch(userAccounts.get(userAddress)) {
            case (?userAccount) {
                userAccount
            };
            case _ {
                let newUserAccount: UserAccount = {
                    user = caller;
                    code = createUserCode();
                    nickname = userAddress;
                    following = [];
                    followers = [];
                };
                userAccounts.put(userAddress, newUserAccount);
                newUserAccount
            };
        }
    };

    public shared(msg) func verify(verifyType: VerifyType, account: Text) : async Result.Result<Bool, Text> {
        let userAddress: Text = PrincipalUtils.toAddress(msg.caller);
        switch(users.get(userAddress)) {
            case (?user) {
                var twitter: ?Text = user.twitter;
                var twitterTime: ?Int = user.twitterTime;
                var discord: ?Text = user.discord;
                var discordTime: ?Int = user.discordTime;
                var github: ?Text = user.github;
                var githubTime: ?Int = user.githubTime;
                switch(verifyType) {
                    case (#twitter) {
                        let twitterAccount: Text = "@" # account;
                        twitter := ?twitterAccount;
                        twitterTime := ?Time.now();
                    };
                    case (#discord) {
                        discord := ?account;
                        discordTime := ?Time.now();
                    };
                    case (#github) {
                        github := ?account;
                        githubTime := ?Time.now();
                    };
                };
                let newUser: User = {
                    user = user.user;
                    account = user.account;
                    principalId = user.principalId;
                    twitter = twitter;
                    twitterTime = twitterTime;
                    discord = discord;
                    discordTime = discordTime;
                    github = github;
                    githubTime = githubTime;
                };
                users.put(userAddress, newUser);
            };
            case _ {
                var twitter: ?Text = null;
                var twitterTime: ?Int = null;
                var discord: ?Text = null;
                var discordTime: ?Int = null;
                var github: ?Text = null;
                var githubTime: ?Int = null;
                switch(verifyType) {
                    case (#twitter) {
                        let twitterAccount: Text = "@" # account;
                        twitter := ?twitterAccount;
                        twitterTime := ?Time.now();
                    };
                    case (#discord) {
                        discord := ?account;
                        discordTime := ?Time.now();
                    };
                    case (#github) {
                        github := ?account;
                        githubTime := ?Time.now();
                    };
                };
                let newUser: User = {
                    user = msg.caller;
                    account = PrincipalUtils.toAddress(msg.caller);
                    principalId = PrincipalUtils.toText(msg.caller);
                    twitter = twitter;
                    twitterTime = twitterTime;
                    discord = discord;
                    discordTime = discordTime;
                    github = github;
                    githubTime = githubTime;
                };
                users.put(userAddress, newUser);
            };
        };
        return #ok(true);
    };

    public shared(msg) func updateNickname(nickname: Text) : async Result.Result<Bool, Text> {
        let userAddress: Text = PrincipalUtils.toAddress(msg.caller);
        let userAccount: UserAccount = getUserAccount(msg.caller);
        let newUserAccount: UserAccount = {
            user = userAccount.user;
            code = userAccount.code;
            nickname = nickname;
            following = userAccount.following;
            followers = userAccount.followers;
        };
        userAccounts.put(userAddress, newUserAccount);
        return #ok(true);
    };

    public shared(msg) func addFollowing(followUser: Principal) : async Result.Result<Bool, Text> {
        let userAddress: Text = PrincipalUtils.toAddress(msg.caller);
        let userAccount: UserAccount = getUserAccount(msg.caller);
        let newUserAccount: UserAccount = {
            user = userAccount.user;
            code = userAccount.code;
            nickname = userAccount.nickname;
            following = arrayAdd<Principal>(userAccount.following, followUser);
            followers = userAccount.followers;
        };
        userAccounts.put(userAddress, newUserAccount);

        let followUserAddress: Text = PrincipalUtils.toAddress(followUser);
        let followUserAccount: UserAccount = getUserAccount(followUser);
        let newFollowUserAccount: UserAccount = {
            user = followUserAccount.user;
            code = followUserAccount.code;
            nickname = followUserAccount.nickname;
            following = followUserAccount.following;
            followers = arrayAdd<Principal>(followUserAccount.followers, msg.caller);
        };
        userAccounts.put(followUserAddress, newFollowUserAccount);
        return #ok(true);
    };

    public shared(msg) func deleteFollowing(followUser: Principal) : async Result.Result<Bool, Text> {
        let userAddress: Text = PrincipalUtils.toAddress(msg.caller);
        let userAccount: UserAccount = getUserAccount(msg.caller);
        let newUserAccount: UserAccount = {
            user = userAccount.user;
            code = userAccount.code;
            nickname = userAccount.nickname;
            following = CollectUtils.arrayRemove<Principal>(userAccount.following, followUser, func(a, b) : Bool {
                a == b
            });
            followers = userAccount.followers;
        };
        userAccounts.put(userAddress, newUserAccount);

        let followUserAddress: Text = PrincipalUtils.toAddress(followUser);
        let followUserAccount: UserAccount = getUserAccount(followUser);
        let newFollowUserAccount: UserAccount = {
            user = followUserAccount.user;
            code = followUserAccount.code;
            nickname = followUserAccount.nickname;
            following = followUserAccount.following;
            followers = CollectUtils.arrayRemove<Principal>(followUserAccount.followers, msg.caller, func(a, b) : Bool {
                a == b
            });
        };
        userAccounts.put(followUserAddress, newFollowUserAccount);
        return #ok(true);
    };

    public query(msg) func get(caller: Principal) : async Result.Result<User, Text> {
        let userAddress: Text = PrincipalUtils.toAddress(caller);
        let user: User = switch(users.get(userAddress)) {
            case (?user) {
                user
            };
            case _ {
                let newUser: User = {
                    user = msg.caller;
                    account = PrincipalUtils.toAddress(msg.caller);
                    principalId = PrincipalUtils.toText(msg.caller);
                    twitter = null;
                    twitterTime = null;
                    discord = null;
                    discordTime = null;
                    github = null;
                    githubTime = null;
                };
                newUser
            };
        };
        return #ok(user);
    };

    public shared(msg) func getAccount() : async Result.Result<UserAccountResponse, Text> {
        let userAccount: UserAccount = getUserAccount(msg.caller);
        return #ok({
            user = userAccount.user;
            code = userAccount.code;
            nickname = userAccount.nickname;
            following = userAccount.following.size();
            followers = userAccount.followers.size();
        });
    };

    public query(msg) func findUser(user: Principal) : async Result.Result<?User, Text> {
        let userAddress: Text = PrincipalUtils.toAddress(user);
        return #ok(users.get(userAddress));
    };

    public query(msg) func findAccount(user: Principal) : async Result.Result<?UserAccountResponse, Text> {
        let userAddress: Text = PrincipalUtils.toAddress(user);
        switch(userAccounts.get(userAddress)) {
            case (?userAccount) {
                #ok(?{
                    user = userAccount.user;
                    code = userAccount.code;
                    nickname = userAccount.nickname;
                    following = userAccount.following.size();
                    followers = userAccount.followers.size();
                });
            };
            case _ {
                #ok(null);
            };
        }
    };

    public query(msg) func findFollowing(user: Principal) : async Result.Result<[Principal], Text> {
        let userAddress: Text = PrincipalUtils.toAddress(user);
        switch(userAccounts.get(userAddress)) {
            case (?userAccount) {
                #ok(userAccount.following);
            };
            case _ {
                #ok([]);
            };
        }
    };

    public query(msg) func findFollower(user: Principal) : async Result.Result<[Principal], Text> {
        let userAddress: Text = PrincipalUtils.toAddress(user);
        switch(userAccounts.get(userAddress)) {
            case (?userAccount) {
                #ok(userAccount.followers);
            };
            case _ {
                #ok([]);
            };
        }
    };

    // public shared(msg) func updateTwitter() : async () {
    //     for ((userAddress, user) in users.entries()) {
    //         switch(user.twitter) {
    //             case (?twitter) {
    //                 let twitterAccount: Text = "@" # twitter;
    //                 let newUser: User = {
    //                     user = user.user;
    //                     account = user.account;
    //                     principalId = user.principalId;
    //                     twitter = ?twitterAccount;
    //                     twitterTime = user.twitterTime;
    //                     discord = user.discord;
    //                     discordTime = user.discordTime;
    //                     github = user.github;
    //                     githubTime = user.githubTime;
    //                 };
    //                 users.put(userAddress, newUser);
    //             };
    //             case _ {};
    //         };
    //     };
    // };

    public query(msg) func find(offset: Nat, limit: Nat) : async Result.Result<[User], Text> {
        var buffer : Buffer.Buffer<User> = Buffer.Buffer<User>(0);
        let start : Nat = (offset - 1) * limit;
        let end : Nat = offset * limit;
        var index : Nat = 0;
        label l for ((userAddress, user) in users.entries()) {
            if (index >= start and index < end) {
                buffer.add(user);
            };
            index += 1;
            if (index >= end) {
                break l;
            };
        };
        return #ok(buffer.toArray());
    };

    public query func searchIcpAccount(icpAccount: Text) : async Result.Result<[User], Text> {
        switch(users.get(icpAccount)) {
            case (?user) {
                return #ok([user]);
            };
            case _ {
                for ((userAddress, user) in users.entries()) {
                    if (icpAccount == user.principalId) {
                        return #ok([user]);
                    };
                };
            };
        };
        return #ok([]);
    };

    public query func searchTwitterAccount(twitterAccount: Text) : async Result.Result<[User], Text> {
        let account: Text = "@" # twitterAccount;
        var matchUsers: Buffer.Buffer<User> = Buffer.Buffer<User>(0);
        for ((userAddress, user) in users.entries()) {
            switch(user.twitter) {
                case (?twitter) {
                    if (account == twitter) {
                        matchUsers.add(user);
                    };
                };
                case _ {};
            };
        };
        return #ok(matchUsers.toArray());
    };

    public query func searchMultiTwitterAccount(twitterAccounts: [Text]) : async Result.Result<[User], Text> {
        var accounts: Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
        for (twitterAccount in twitterAccounts.vals()) {
            accounts.add("@" # twitterAccount);
        };
        var matchUsers: Buffer.Buffer<User> = Buffer.Buffer<User>(0);
        for ((userAddress, user) in users.entries()) {
            switch(user.twitter) {
                case (?twitter) {
                    if (CollectUtils.arrayContains<Text>(accounts.toArray(), twitter, func (twitterAccount: Text, twitter: Text): Bool {
                        return twitterAccount == twitter;
                    })) {
                        matchUsers.add(user);
                    };
                };
                case _ {};
            };
        };
        return #ok(matchUsers.toArray());
    };

    public query func searchDiscordAccount(discordAccount: Text) : async Result.Result<[User], Text> {
        var matchUsers: Buffer.Buffer<User> = Buffer.Buffer<User>(0);
        for ((userAddress, user) in users.entries()) {
            switch(user.discord) {
                case (?discord) {
                    if (discordAccount == discord) {
                        matchUsers.add(user);
                    };
                };
                case _ {};
            };
        };
        return #ok(matchUsers.toArray());
    };

    public query func searchGithubAccount(githubAccount: Text) : async Result.Result<[User], Text> {
        var matchUsers: Buffer.Buffer<User> = Buffer.Buffer<User>(0);
        for ((userAddress, user) in users.entries()) {
            switch(user.github) {
                case (?github) {
                    if (githubAccount == github) {
                        matchUsers.add(user);
                    };
                };
                case _ {};
            };
        };
        return #ok(matchUsers.toArray());
    };

    public query func cycleBalance() : async Result.Result<Nat, Text> {
        return #ok(ExperimentalCycles.balance());
    };
    
    public shared(msg) func cycleAvailable() : async Result.Result<Nat, Text> {
        return #ok(ExperimentalCycles.available());
    };

    /*
    * upgrade functions
    */
    system func preupgrade() {
        userArray := Iter.toArray(users.entries());
        userAccountArray := Iter.toArray(userAccounts.entries());
    };

    system func postupgrade() {
        users := HashMap.fromIter<Text, User>(userArray.vals(), 1, Text.equal, Text.hash);
        userAccounts := HashMap.fromIter<Text, UserAccount>(userAccountArray.vals(), 1, Text.equal, Text.hash);
    };
}