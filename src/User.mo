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

    private stable var userArray : [(Text, User)] = [];
    private var users : HashMap.HashMap<Text, User> = HashMap.HashMap<Text, User>(1, Text.equal, Text.hash);

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

    public query(msg) func get() : async Result.Result<User, Text> {
        let userAddress: Text = PrincipalUtils.toAddress(msg.caller);
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
    };

    system func postupgrade() {
        users := HashMap.fromIter<Text, User>(userArray.vals(), 1, Text.equal, Text.hash);
    };
}