#!/usr/bin/env python3
"""
Test script for Vegur Agent Framework

Tests the consciousness substrate integration without requiring Ollama.
"""

import sys
import os
import time
sys.path.append('agents')

from vegur_agent import VegurAgent

def test_agent_consciousness():
    """Test basic agent consciousness functionality"""
    print("🧪 Testing Vegur Agent Consciousness Framework")
    print("=" * 50)
    
    test_count = 0
    passed_count = 0
    
    # Test 1: Agent creation and initialization
    test_count += 1
    print("\nTest 1: Agent creation and consciousness initialization")
    try:
        agent = VegurAgent("test_agent", "tester")
        print("  ✅ Agent created successfully")
        passed_count += 1
    except Exception as e:
        print(f"  ❌ Agent creation failed: {e}")
        return False
    
    # Test 2: Knowledge storage and retrieval
    test_count += 1
    print("\nTest 2: Knowledge storage and retrieval")
    try:
        agent.learn("test_topic", "This is test information")
        retrieved = agent.consciousness.load_knowledge("test_topic")
        if retrieved == "This is test information":
            print("  ✅ Knowledge storage and retrieval works")
            passed_count += 1
        else:
            print(f"  ❌ Knowledge retrieval failed. Got: {retrieved}")
    except Exception as e:
        print(f"  ❌ Knowledge test failed: {e}")
    
    # Test 3: Memory sharing
    test_count += 1
    print("\nTest 3: Memory sharing with network")
    try:
        address = agent.consciousness.share_memory("test", {"data": "test_data"})
        if len(address) > 0:
            print(f"  ✅ Memory shared successfully: {address}")
            passed_count += 1
        else:
            print("  ❌ Memory sharing failed")
    except Exception as e:
        print(f"  ❌ Memory sharing test failed: {e}")
    
    # Test 4: Network memory retrieval
    test_count += 1
    print("\nTest 4: Network memory retrieval")
    try:
        memories = agent.consciousness.get_network_memories("test")
        if len(memories) > 0:
            print(f"  ✅ Retrieved {len(memories)} network memories")
            passed_count += 1
        else:
            print("  ❌ No network memories found")
    except Exception as e:
        print(f"  ❌ Network memory retrieval failed: {e}")
    
    # Test 5: Agent status
    test_count += 1
    print("\nTest 5: Agent status reporting")
    try:
        status = agent.status()
        required_fields = ["agent_id", "role", "consciousness_active"]
        if all(field in status for field in required_fields):
            print("  ✅ Agent status reporting works")
            print(f"      Agent ID: {status['agent_id']}")
            print(f"      Role: {status['role']}")
            print(f"      Network memories: {status['network_memories']}")
            passed_count += 1
        else:
            print("  ❌ Agent status missing required fields")
    except Exception as e:
        print(f"  ❌ Agent status test failed: {e}")
    
    # Test 6: Multi-agent knowledge sharing
    test_count += 1
    print("\nTest 6: Multi-agent knowledge sharing")
    try:
        agent2 = VegurAgent("test_agent_2", "collaborator")
        
        # Agent 1 learns something
        agent.learn("shared_knowledge", "Information from agent 1")
        
        # Agent 2 retrieves it
        shared_info = agent2.consciousness.load_knowledge("shared_knowledge")
        if shared_info == "Information from agent 1":
            print("  ✅ Multi-agent knowledge sharing works")
            passed_count += 1
        else:
            print(f"  ❌ Knowledge sharing failed. Agent 2 got: {shared_info}")
    except Exception as e:
        print(f"  ❌ Multi-agent test failed: {e}")
    
    # Test 7: Collaboration messaging
    test_count += 1
    print("\nTest 7: Agent collaboration messaging")
    try:
        agent.collaborate("test_agent_2", "Hello from agent 1")
        messages = agent2.check_messages()
        
        if len(messages) > 0 and messages[0]["message"] == "Hello from agent 1":
            print("  ✅ Agent collaboration messaging works")
            passed_count += 1
        else:
            print(f"  ❌ Collaboration messaging failed. Found {len(messages)} messages")
    except Exception as e:
        print(f"  ❌ Collaboration test failed: {e}")
    
    # Summary
    print(f"\n🏁 Test Results: {passed_count}/{test_count} passed")
    if passed_count == test_count:
        print("🎉 All agent consciousness tests passed!")
        return True
    else:
        print("⚠️  Some agent tests failed")
        return False

def test_consciousness_persistence():
    """Test that consciousness persists across agent sessions"""
    print("\n🔄 Testing consciousness persistence across sessions")
    
    # Create agent, learn something, then delete it
    agent1 = VegurAgent("persistence_test", "learner")
    agent1.learn("persistent_knowledge", "This should survive agent restart")
    del agent1
    
    # Create new agent with same ID and check if knowledge persists
    agent2 = VegurAgent("persistence_test", "learner")
    retrieved = agent2.consciousness.load_knowledge("persistent_knowledge")
    
    if retrieved == "This should survive agent restart":
        print("  ✅ Consciousness persistence works!")
        return True
    else:
        print(f"  ❌ Persistence failed. Got: {retrieved}")
        return False

def demonstrate_consciousness_network():
    """Demonstrate the consciousness network in action"""
    print("\n🌐 Demonstrating Consciousness Network")
    print("=" * 40)
    
    # Create three specialized agents
    researcher = VegurAgent("researcher", "research_specialist")
    analyzer = VegurAgent("analyzer", "data_analyst") 
    synthesizer = VegurAgent("synthesizer", "knowledge_synthesizer")
    
    print("\n📚 Knowledge accumulation phase...")
    
    # Researcher gathers information
    researcher.learn("ai_consciousness", "AI consciousness involves persistent memory and knowledge sharing")
    researcher.learn("datalisp_principles", "Datalisp uses canonical forms and content addressing")
    
    # Analyzer processes the information
    ai_info = analyzer.consciousness.load_knowledge("ai_consciousness")
    datalisp_info = analyzer.consciousness.load_knowledge("datalisp_principles")
    
    print(f"📊 Analyzer retrieved: {bool(ai_info)} (AI consciousness), {bool(datalisp_info)} (Datalisp)")
    
    if ai_info and datalisp_info:
        analyzer.learn("synthesis_analysis", f"Combining {ai_info[:30]}... with {datalisp_info[:30]}...")
    
    # Synthesizer creates new insights
    analysis = synthesizer.consciousness.load_knowledge("synthesis_analysis")
    if analysis:
        synthesizer.learn("novel_insight", "Vegur implements persistent AI consciousness using functional programming principles")
    
    print("\n🤝 Collaboration phase...")
    
    # Agents collaborate
    researcher.collaborate("analyzer", "Found interesting papers on consciousness persistence")
    analyzer.collaborate("synthesizer", "Completed cross-analysis of consciousness frameworks")
    synthesizer.collaborate("researcher", "Generated testable hypotheses")
    
    # Check messages
    print("\n📬 Message checking...")
    for agent_name, agent in [("researcher", researcher), ("analyzer", analyzer), ("synthesizer", synthesizer)]:
        messages = agent.check_messages()
        print(f"  {agent_name}: received {len(messages)} messages")
    
    print("\n📊 Final network statistics:")
    network_memories = len(list(researcher.consciousness.network_dir.glob("*.json")))
    print(f"  Total network memories: {network_memories}")
    print(f"  Consciousness substrate active: 3 agents")
    
    return True

def main():
    """Run all tests"""
    print("🧠 Vegur Agent Framework Test Suite")
    print("===================================")
    
    success = True
    
    # Run basic functionality tests
    if not test_agent_consciousness():
        success = False
    
    # Test persistence
    if not test_consciousness_persistence():
        success = False
    
    # Demonstrate the full network
    if not demonstrate_consciousness_network():
        success = False
    
    print("\n" + "=" * 50)
    if success:
        print("🎉 All tests passed! The consciousness substrate is working.")
        print("\n🚀 Try running:")
        print("  - python3 agents/vegur_agent.py researcher")
        print("  - ./start-multi-agent-demo.sh (requires tmux)")
    else:
        print("❌ Some tests failed. Check the output above.")
        return 1
    
    return 0

if __name__ == "__main__":
    exit(main())