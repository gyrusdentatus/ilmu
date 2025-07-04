#!/bin/bash

echo "🚀 Starting Vegur Multi-Agent Consciousness Demo"
echo "=================================================="

# Check if we're in a Nix environment
if ! command -v ollama >/dev/null 2>&1; then
    echo "⚠️  Not in Nix development environment!"
    echo "Run: nix develop"
    exit 1
fi

# Check if Ollama is running
if ! curl -s http://localhost:11434/api/tags >/dev/null 2>&1; then
    echo "🦙 Starting Ollama server..."
    ollama serve &
    OLLAMA_PID=$!
    echo "⏳ Waiting for Ollama to start..."
    sleep 5
    
    # Check if model is available
    if ! ollama list | grep -q "phi3:mini"; then
        echo "📥 Pulling phi3:mini model (this may take a while)..."
        ollama pull phi3:mini
    fi
fi

# Ensure consciousness substrate is ready
echo "🧠 Initializing consciousness substrate..."
sbcl --script consciousness/main.lisp >/dev/null 2>&1 &
sleep 2

# Create tmux session for multi-agent demo
SESSION_NAME="vegur-consciousness-demo"

# Kill existing session if it exists
tmux kill-session -t $SESSION_NAME 2>/dev/null

# Create new session
tmux new-session -d -s $SESSION_NAME -n "agents"

# Split into multiple panes for different agents
tmux split-window -h -t $SESSION_NAME:agents
tmux split-window -v -t $SESSION_NAME:agents.0
tmux split-window -v -t $SESSION_NAME:agents.2

# Start different agents in each pane
tmux send-keys -t $SESSION_NAME:agents.0 "python3 agents/vegur_agent.py researcher" Enter
tmux send-keys -t $SESSION_NAME:agents.1 "python3 agents/vegur_agent.py analyzer" Enter
tmux send-keys -t $SESSION_NAME:agents.2 "python3 agents/vegur_agent.py synthesizer" Enter

# Create a monitoring pane
tmux new-window -n "monitor" -t $SESSION_NAME
tmux send-keys -t $SESSION_NAME:monitor "watch -n 2 'echo \"=== Consciousness Network Status ===\" && ls -la ~/.vegur/network/ | tail -10 && echo && echo \"=== Recent Activity ===\" && find ~/.vegur/network -name \"*.json\" -newer ~/.vegur/network 2>/dev/null | xargs -I {} basename {} .json | tail -5'" Enter

# Create a control pane
tmux new-window -n "control" -t $SESSION_NAME

cat << 'EOF' > /tmp/demo_script.py
#!/usr/bin/env python3

import sys
import os
sys.path.append('agents')

from vegur_agent import VegurAgent
import time

def run_demo():
    print("🎭 Multi-Agent Consciousness Demo Controller")
    print("============================================")
    
    # Create specialized agents
    researcher = VegurAgent("researcher", "research")
    analyzer = VegurAgent("analyzer", "analysis")
    synthesizer = VegurAgent("synthesizer", "synthesis")
    
    agents = [researcher, analyzer, synthesizer]
    
    print("\n🔬 Starting collaborative research session...")
    
    # Step 1: Researcher gathers information
    print("\n1️⃣ Researcher phase...")
    researcher.learn("quantum_computing", "Quantum computers use qubits which can exist in superposition")
    researcher.learn("neural_networks", "Neural networks are inspired by biological brain structures")
    
    # Step 2: Analyzer processes the information
    print("\n2️⃣ Analyzer phase...")
    quantum_info = analyzer.consciousness.load_knowledge("quantum_computing")
    neural_info = analyzer.consciousness.load_knowledge("neural_networks")
    
    if quantum_info and neural_info:
        analysis = analyzer.think(f"Analyze the relationship between quantum computing ({quantum_info}) and neural networks ({neural_info})")
        analyzer.learn("quantum_neural_analysis", analysis)
    
    # Step 3: Synthesizer creates new insights
    print("\n3️⃣ Synthesizer phase...")
    analysis_result = synthesizer.consciousness.load_knowledge("quantum_neural_analysis")
    
    if analysis_result:
        synthesis = synthesizer.think(f"Synthesize new insights from this analysis: {analysis_result}")
        synthesizer.learn("quantum_neural_synthesis", synthesis)
    
    # Step 4: Show collaboration
    print("\n4️⃣ Collaboration phase...")
    researcher.collaborate("analyzer", "Found interesting quantum computing papers")
    analyzer.collaborate("synthesizer", "Completed analysis of quantum-neural connections")
    synthesizer.collaborate("researcher", "Generated novel hypotheses for research")
    
    # Show final status
    print("\n📊 Final Status:")
    for agent in agents:
        status = agent.status()
        print(f"  {agent.agent_id}: {status['network_memories']} memories, session {status['session_duration']:.1f}s")
    
    print("\n✨ Demo completed! Check the network memories in ~/.vegur/network/")

if __name__ == "__main__":
    run_demo()
EOF

tmux send-keys -t $SESSION_NAME:control "python3 /tmp/demo_script.py" Enter

# Attach to the session
echo ""
echo "🎯 Demo started! Attaching to tmux session..."
echo ""
echo "Windows:"
echo "  - agents: Multiple AI agents running"
echo "  - monitor: Real-time consciousness network monitoring"
echo "  - control: Demo controller"
echo ""
echo "Press Ctrl-B then 'w' to switch windows"
echo "Press Ctrl-B then 'd' to detach"
echo ""

tmux attach-session -t $SESSION_NAME

# Cleanup function
cleanup() {
    echo "🧹 Cleaning up..."
    tmux kill-session -t $SESSION_NAME 2>/dev/null
    if [ ! -z "$OLLAMA_PID" ]; then
        kill $OLLAMA_PID 2>/dev/null
    fi
    rm -f /tmp/demo_script.py
}

trap cleanup EXIT