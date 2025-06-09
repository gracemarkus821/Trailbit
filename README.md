# 🥾 Trailbit - Open Trail Rewards

A decentralized platform that incentivizes trail discovery and maintenance through token rewards. Earn TRAIL tokens by exploring new trails and keeping them in good condition! 🌲

## 🌟 Features

- **🗺️ Trail Creation**: Register new trails with custom reward pools
- **🎯 Discovery Rewards**: Earn tokens for visiting trails (once per day)
- **🔧 Maintenance Rewards**: Get rewarded for trail upkeep (once per week)
- **💰 Community Funding**: Anyone can fund trail reward pools
- **📊 User Statistics**: Track your trail activities and earnings

## 🚀 Getting Started

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd trailbit
clarinet check
```

## 📖 Usage

### Creating a Trail
```clarity
(contract-call? .Trailbit create-trail 
  "Mountain Peak Trail" 
  "Rocky Mountains, CO" 
  u3 
  u1000000 
  u100 
  u200)
```

### Visiting a Trail
```clarity
(contract-call? .Trailbit visit-trail u1)
```

### Maintaining a Trail
```clarity
(contract-call? .Trailbit maintain-trail u1)
```

### Funding a Trail
```clarity
(contract-call? .Trailbit fund-trail u1 u500000)
```

## 🎮 Game Mechanics

- **Discovery Cooldown**: 24 hours (144 blocks) between visits
- **Maintenance Cooldown**: 1 week (1008 blocks) between maintenance
- **Difficulty Levels**: 1-5 scale for trail difficulty
- **Reward Distribution**: Automatic token distribution upon valid actions

## 🔍 Read-Only Functions

- `get-trail(trail-id)` - Get trail information
- `get-balance(account)` - Check token balance
- `get-user-stats(user)` - View user statistics
- `get-trail-visits(trail-id, visitor)` - Check visit history
- `get-trail-maintenance(trail-id, maintainer)` - Check maintenance history

## 🏗️ Contract Structure

### Data Maps
- **trails**: Core trail information and reward pools
- **trail-visits**: User visit tracking and earnings
- **trail-maintenance**: Maintenance records and rewards
- **user-stats**: Comprehensive user activity statistics

### Token Details
- **Name**: Trailbit
- **Symbol**: TRAIL
- **Decimals**: 6
- **Type**: SIP-010 Fungible Token

## 🛡️ Security Features

- Owner-only minting capabilities
- Trail creator permissions for deactivation
- Cooldown periods prevent spam
- Reward pool validation
- Active trail status checks

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Test with Clarinet
4. Submit a pull request

## 📄 License

This project is open source and available under the MIT License.

---

*Happy trails! 🥾✨*




